//! Hatch build hook integration for maturin.
//!
//! This module provides the Rust backend for using maturin as a
//! [Hatch build hook](https://hatch.pypa.io/latest/plugins/build-hook/reference/).
//! It allows projects to use `hatchling` as the build backend while delegating
//! native artifact compilation to maturin.
//!
//! # Architecture
//!
//! The integration consists of two parts:
//!
//! 1. **Python hook** (`maturin/hatch.py`): A Hatch build hook plugin that
//!    implements `BuildHookInterface`. It invokes maturin CLI commands and
//!    merges the results into Hatch's build data.
//!
//! 2. **Rust backend** (this module): Provides two CLI commands under
//!    `maturin pep517`:
//!    - `build-artifacts`: Compiles native extensions and returns file mappings
//!      for wheel builds
//!    - `sdist-augment`: Returns additional files (path dependencies) to include
//!      in source distributions
//!
//! # Usage
//!
//! Users configure the hook in their `pyproject.toml`:
//!
//! ```toml
//! [build-system]
//! requires = ["hatchling", "maturin>=1.0"]
//! build-backend = "hatchling.build"
//!
//! [tool.hatch.build.hooks.maturin]
//! bindings = "pyo3"
//! ```
//!
//! # Communication Protocol
//!
//! The Python hook invokes maturin CLI commands and parses JSON from the last
//! line of stdout. The JSON contains:
//!
//! - `force_include`: Map of source paths to archive-relative target paths
//! - `tag`: Wheel compatibility tag (for wheel builds)
//! - `pure_python`: Always `false` for native builds
//!
//! Binary artifacts are placed in the `.data/scripts/` directory within the
//! wheel via `force_include`.
//!
//! # Path Dependency Handling
//!
//! For source distributions, path dependencies outside the project root are
//! included using the same layout as `maturin sdist`: files are placed relative
//! to the common ancestor of all path dependencies, preserving the original
//! directory structure.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use anyhow::{Context, Result, bail};
use fs_err as fs;
use serde::Serialize;
use tempfile::TempDir;
use toml_edit::{DocumentMut, Item, Table, Value};

use crate::BuildContext;
use crate::BuildOptions;
use crate::PlatformTag;
use crate::PythonInterpreter;
use crate::archive_source::ArchiveSource;
use crate::binding_generator::{
    ArtifactTarget, BinBindingGenerator, BindingGenerator, CffiBindingGenerator, GeneratorOutput,
    Pyo3BindingGenerator, UniFfiBindingGenerator,
};
use crate::bridge::Abi3Version;
use crate::compile::BuildArtifact;
use crate::compile::compile;
use crate::source_distribution::find_path_deps;

/// JSON output for the hatch build hook `pep517 build-artifacts` command.
#[derive(Debug, Serialize)]
pub struct BuildArtifactsOutput {
    /// Wheel tag to pass to the wheel builder.
    pub tag: String,
    /// Indicates the build is not pure Python.
    pub pure_python: bool,
    /// Mapping of source paths to target paths inside the wheel.
    pub force_include: BTreeMap<String, String>,
}

/// JSON output for the hatch build hook `pep517 sdist-augment` command.
#[derive(Debug, Serialize)]
pub struct SdistAugmentOutput {
    /// Mapping of source paths to target paths inside the sdist.
    pub force_include: BTreeMap<String, String>,
}

/// Build native artifacts and return a JSON mapping for the hatch build hook.
pub fn build_artifacts(
    build_options: BuildOptions,
    output_dir: &Path,
    strip: bool,
) -> Result<BuildArtifactsOutput> {
    let mut context = build_options
        .into_build_context()
        .strip(strip)
        .editable(false)
        .build()?;

    if context.cargo_options.profile.is_none() {
        context.cargo_options.profile = Some("release".to_string());
    }

    // Always skip auditwheel for the hatch hook.
    context.auditwheel = crate::auditwheel::AuditWheelMode::Skip;

    fs::create_dir_all(output_dir)?;

    let tag = primary_wheel_tag(&context)?;
    let mut force_include = BTreeMap::new();

    // Scripts directory in the wheel data directory
    let scripts_dir = format!(
        "{}-{}.data/scripts",
        context.metadata24.get_distribution_escaped(),
        context.metadata24.version
    );

    let temp_dir = Rc::new(TempDir::new_in(output_dir)?);
    let module = module_path(&context)?;

    match context.bridge() {
        crate::BridgeModel::PyO3(bindings) => {
            let (artifact, interpreter) = pyo3_artifact(&context, bindings)?;
            let mut generator = Pyo3BindingGenerator::new(
                bindings.abi3.is_some(),
                interpreter.as_ref(),
                temp_dir.clone(),
            )?;
            let output = generator.generate_bindings(&context, &artifact, &module)?;
            collect_bindings(
                output_dir,
                &artifact,
                output,
                &mut force_include,
                &scripts_dir,
            )?;
        }
        crate::BridgeModel::Cffi => {
            let interpreter = context
                .interpreter
                .first()
                .context("A python interpreter is required for cffi builds")?;
            let artifact = context.compile_cdylib(None, None)?;
            let mut generator = CffiBindingGenerator::new(interpreter, temp_dir.clone())?;
            let output = generator.generate_bindings(&context, &artifact, &module)?;
            collect_bindings(
                output_dir,
                &artifact,
                output,
                &mut force_include,
                &scripts_dir,
            )?;
        }
        crate::BridgeModel::UniFfi => {
            let artifact = context.compile_cdylib(None, None)?;
            let mut generator = UniFfiBindingGenerator::default();
            let output = generator.generate_bindings(&context, &artifact, &module)?;
            collect_bindings(
                output_dir,
                &artifact,
                output,
                &mut force_include,
                &scripts_dir,
            )?;
        }
        crate::BridgeModel::Bin(_) => {
            if context.target.is_wasi() {
                bail!("WASI binaries are not supported by the hatch hook");
            }
            let artifacts = compile(&context, None, &context.compile_targets)
                .context("Failed to build a binary through cargo")?;
            if artifacts.is_empty() {
                bail!("Cargo didn't build a binary");
            }
            let mut metadata24 = context.metadata24.clone();
            let mut generator = BinBindingGenerator::new(&mut metadata24);
            for artifact_map in artifacts {
                let artifact = artifact_map
                    .get(&cargo_metadata::CrateType::Bin)
                    .cloned()
                    .context("Cargo didn't build a binary")?;
                let output = generator.generate_bindings(&context, &artifact, Path::new(""))?;
                collect_bindings(
                    output_dir,
                    &artifact,
                    output,
                    &mut force_include,
                    &scripts_dir,
                )?;
            }
        }
    }

    Ok(BuildArtifactsOutput {
        tag,
        pure_python: false,
        force_include,
    })
}

/// Return additional sdist files for local path dependencies.
pub fn sdist_augment(
    mut build_options: BuildOptions,
    output_dir: &Path,
) -> Result<SdistAugmentOutput> {
    if build_options.cargo.features.is_empty() && !build_options.cargo.all_features {
        build_options.cargo.all_features = true;
    }

    let context = build_options
        .into_build_context()
        .strip(false)
        .editable(false)
        .sdist_only(true)
        .build()?;

    fs::create_dir_all(output_dir)?;

    let project_root = &context.project_layout.project_root;
    let path_deps = find_path_deps(&context.cargo_metadata)?;
    if path_deps.is_empty() {
        return Ok(SdistAugmentOutput {
            force_include: BTreeMap::new(),
        });
    }

    // Find the common ancestor of project root, workspace root, and all path deps
    // This matches how `maturin sdist` determines the sdist root
    let workspace_root = context.cargo_metadata.workspace_root.as_std_path();
    let mut sdist_root =
        common_path(project_root, workspace_root).unwrap_or_else(|| project_root.to_path_buf());
    for path_dep in path_deps.values() {
        let dep_dir = path_dep.manifest_path.parent().unwrap();
        if let Some(prefix) = common_path(&sdist_root, dep_dir) {
            sdist_root = prefix;
        } else {
            bail!(
                "Failed to find common path prefix between {} and {}",
                sdist_root.display(),
                dep_dir.display()
            );
        }
    }

    let mut dep_dest_map = HashMap::new();
    for path_dep in path_deps.values() {
        let dep_root = path_dep.manifest_path.parent().unwrap();
        let dep_root_abs = normalize_path(dep_root);
        let dest_base = dep_root
            .strip_prefix(&sdist_root)
            .unwrap_or(dep_root)
            .to_path_buf();
        dep_dest_map.insert(dep_root_abs, dest_base);
    }

    // Determine where the main crate sits relative to sdist_root
    let main_crate_prefix = project_root
        .strip_prefix(&sdist_root)
        .unwrap_or(Path::new(""))
        .to_path_buf();

    let mut force_include = BTreeMap::new();
    let mut force_targets = HashSet::new();

    // Rewrite the main Cargo.toml if any path deps need relocation.
    let main_rewrite = rewrite_manifest_paths(
        &context.manifest_path,
        &main_crate_prefix,
        &dep_dest_map,
        None,
        false,
    )?;
    if let Some(rewritten) = main_rewrite {
        let target = main_crate_prefix.join("Cargo.toml");
        let output_path = write_generated_file(output_dir, &target, rewritten.as_bytes())?;
        insert_mapping_unique(
            &mut force_include,
            &mut force_targets,
            &output_path,
            &target,
        )?;
    }

    for path_dep in path_deps.values() {
        let dep_root = path_dep.manifest_path.parent().unwrap();
        let dep_root_abs = normalize_path(dep_root);
        let dest_base = dep_dest_map
            .get(&dep_root_abs)
            .context("Missing path dependency destination mapping")?;

        // Ensure Cargo.toml is rewritten for dependency path relocations.
        let readme_path = path_dep.readme.as_ref().map(|readme| dep_root.join(readme));
        let readme_name = readme_path
            .as_ref()
            .and_then(|readme| readme.file_name())
            .map(|name| name.to_string_lossy().to_string());

        let rewrite = rewrite_manifest_paths(
            &path_dep.manifest_path,
            dest_base,
            &dep_dest_map,
            readme_name.as_deref(),
            true,
        )?;
        if let Some(rewritten) = rewrite {
            let target = dest_base.join("Cargo.toml");
            let output_path = write_generated_file(output_dir, &target, rewritten.as_bytes())?;
            insert_mapping_unique(
                &mut force_include,
                &mut force_targets,
                &output_path,
                &target,
            )?;
        }

        if let (Some(readme_path), Some(readme_name)) = (readme_path, readme_name) {
            let target = dest_base.join(&readme_name);
            insert_mapping_unique(
                &mut force_include,
                &mut force_targets,
                &readme_path,
                &target,
            )?;
        }

        let file_list = cargo_package_file_list(&path_dep.manifest_path)?;
        for (target, source) in file_list {
            let dest = dest_base.join(target);
            insert_mapping_unique(&mut force_include, &mut force_targets, &source, &dest)?;
        }
    }

    // Include workspace manifests that are outside the project root.
    let mut workspace_roots = HashSet::new();
    workspace_roots.insert(
        context
            .cargo_metadata
            .workspace_root
            .clone()
            .into_std_path_buf(),
    );
    for dep in path_deps.values() {
        workspace_roots.insert(dep.workspace_root.clone());
    }
    for ws_root in workspace_roots {
        if ws_root.starts_with(project_root) {
            continue;
        }
        let dest_base = ws_root
            .strip_prefix(&sdist_root)
            .unwrap_or(&ws_root)
            .to_path_buf();
        let manifest_path = ws_root.join("Cargo.toml");
        if manifest_path.is_file() {
            let target = dest_base.join("Cargo.toml");
            insert_mapping_unique(
                &mut force_include,
                &mut force_targets,
                &manifest_path,
                &target,
            )?;
        }
    }

    Ok(SdistAugmentOutput { force_include })
}

fn pyo3_artifact(
    context: &BuildContext,
    bindings: &crate::PyO3,
) -> Result<(BuildArtifact, Option<PythonInterpreter>)> {
    let interpreter = if bindings.abi3.is_some() {
        context.interpreter.first().cloned()
    } else {
        Some(
            context
                .interpreter
                .first()
                .cloned()
                .context("A python interpreter is required for non-abi3 builds")?,
        )
    };
    let artifact = context.compile_cdylib(
        interpreter.as_ref(),
        Some(&context.project_layout.extension_name),
    )?;
    Ok((artifact, interpreter))
}

fn module_path(context: &BuildContext) -> Result<PathBuf> {
    let base_path = context
        .project_layout
        .python_module
        .as_ref()
        .map(|python_module| python_module.parent().unwrap().to_path_buf());

    Ok(match &base_path {
        Some(base_path) => context
            .project_layout
            .rust_module
            .strip_prefix(base_path)
            .unwrap()
            .to_path_buf(),
        None => PathBuf::from(&context.project_layout.extension_name),
    })
}

fn collect_bindings(
    output_dir: &Path,
    artifact: &BuildArtifact,
    output: GeneratorOutput,
    force_include: &mut BTreeMap<String, String>,
    scripts_dir: &str,
) -> Result<()> {
    let GeneratorOutput {
        artifact_target,
        artifact_source_override,
        additional_files,
    } = output;

    let artifact_source = artifact_source_override.unwrap_or_else(|| artifact.path.clone());

    match artifact_target {
        ArtifactTarget::ExtensionModule(target) => {
            insert_mapping(force_include, &artifact_source, &target)?;
        }
        ArtifactTarget::Binary(target) => {
            // Place binaries in the .data/scripts/ directory
            let file_name = target
                .file_name()
                .context("Binary artifact target missing filename")?;
            let scripts_target = PathBuf::from(scripts_dir).join(file_name);
            insert_mapping(force_include, &artifact_source, &scripts_target)?;
        }
    }

    if let Some(additional_files) = additional_files {
        for (target, source) in additional_files {
            match source {
                ArchiveSource::Generated(source) => {
                    let output_path = write_generated_file(output_dir, &target, &source.data)?;
                    insert_mapping(force_include, &output_path, &target)?;
                }
                ArchiveSource::File(source) => {
                    insert_mapping(force_include, &source.path, &target)?;
                }
            }
        }
    }

    Ok(())
}

fn insert_mapping_unique(
    map: &mut BTreeMap<String, String>,
    targets: &mut HashSet<PathBuf>,
    source: &Path,
    target: &Path,
) -> Result<()> {
    if !targets.insert(target.to_path_buf()) {
        return Ok(());
    }
    insert_mapping(map, source, target)
}

fn insert_mapping(map: &mut BTreeMap<String, String>, source: &Path, target: &Path) -> Result<()> {
    let source = source.to_string_lossy().to_string();
    let target = target.to_string_lossy().replace('\\', "/");
    if let Some(existing) = map.insert(source.clone(), target.clone()) {
        if existing != target {
            bail!(
                "Duplicate mapping for {}: {} vs {}",
                source,
                existing,
                target
            );
        }
    }
    Ok(())
}

fn primary_wheel_tag(context: &BuildContext) -> Result<String> {
    let platform_tags = if context.platform_tag.is_empty() {
        if context.target.is_linux() {
            vec![PlatformTag::Linux]
        } else {
            Vec::new()
        }
    } else {
        context.platform_tag.clone()
    };
    match context.bridge() {
        crate::BridgeModel::PyO3(bindings) | crate::BridgeModel::Bin(Some(bindings)) => {
            match bindings.abi3 {
                Some(Abi3Version::Version(major, minor)) => {
                    let platform = context.get_platform_tag(&platform_tags)?;
                    Ok(format!("cp{major}{minor}-abi3-{platform}"))
                }
                Some(Abi3Version::CurrentPython) => {
                    let interp = context
                        .interpreter
                        .first()
                        .context("Expected a python interpreter for abi3 tag")?;
                    let platform = context.get_platform_tag(&platform_tags)?;
                    Ok(format!(
                        "cp{major}{minor}-abi3-{platform}",
                        major = interp.major,
                        minor = interp.minor
                    ))
                }
                None => {
                    let interp = context
                        .interpreter
                        .first()
                        .context("Expected a python interpreter for non-abi3 tag")?;
                    interp.get_tag(context, &platform_tags)
                }
            }
        }
        crate::BridgeModel::Bin(None) => {
            let (tag, _) = context.get_universal_tags(&platform_tags)?;
            Ok(tag)
        }
        crate::BridgeModel::Cffi | crate::BridgeModel::UniFfi => {
            let (tag, _) = context.get_universal_tags(&platform_tags)?;
            Ok(tag)
        }
    }
}

fn write_generated_file(output_dir: &Path, target: &Path, data: &[u8]) -> Result<PathBuf> {
    let output_path = output_dir.join(target);
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&output_path, data)?;
    Ok(output_path)
}

fn cargo_package_file_list(manifest_path: &Path) -> Result<Vec<(PathBuf, PathBuf)>> {
    let args = ["package", "--list", "--allow-dirty", "--manifest-path"];
    let output = std::process::Command::new("cargo")
        .args(args)
        .arg(manifest_path)
        .output()
        .with_context(|| {
            format!(
                "Failed to run `cargo package --list --allow-dirty --manifest-path {}`",
                manifest_path.display()
            )
        })?;
    if !output.status.success() {
        bail!(
            "Failed to query file list from cargo: {}\n--- Manifest path: {}\n--- Stdout:\n{}\n--- Stderr:\n{}",
            output.status,
            manifest_path.display(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
    }

    let manifest_dir = manifest_path.parent().unwrap();
    let file_list = String::from_utf8(output.stdout).context("Cargo printed invalid utf-8")?;

    let mut files = Vec::new();
    for relative in file_list.lines() {
        if relative == "Cargo.toml.orig" || relative == "Cargo.toml" {
            continue;
        }
        if matches!(Path::new(relative).extension(), Some(ext) if ext == "pyc" || ext == "pyd" || ext == "so")
        {
            continue;
        }
        let source = manifest_dir.join(relative);
        files.push((PathBuf::from(relative), source));
    }
    Ok(files)
}

fn rewrite_manifest_paths(
    manifest_path: &Path,
    manifest_dest_base: &Path,
    dep_dest_map: &HashMap<PathBuf, PathBuf>,
    readme_name: Option<&str>,
    always_output: bool,
) -> Result<Option<String>> {
    let mut document = parse_toml_file(manifest_path)?;
    let manifest_dir = manifest_path.parent().unwrap();

    if let Some(readme_name) = readme_name {
        rewrite_cargo_toml_readme(&mut document, manifest_path, Some(readme_name))?;
    }

    let changed = rewrite_dependency_paths(
        &mut document,
        manifest_dir,
        manifest_dest_base,
        dep_dest_map,
    )?;

    if always_output || changed || readme_name.is_some() {
        Ok(Some(document.to_string()))
    } else {
        Ok(None)
    }
}

fn parse_toml_file(path: &Path) -> Result<DocumentMut> {
    let text = fs::read_to_string(path)?;
    let document = text
        .parse::<DocumentMut>()
        .context(format!("Failed to parse Cargo.toml at {}", path.display()))?;
    Ok(document)
}

fn rewrite_cargo_toml_readme(
    document: &mut DocumentMut,
    manifest_path: &Path,
    readme_name: Option<&str>,
) -> Result<()> {
    if let Some(readme_name) = readme_name {
        let project = document.get_mut("package").with_context(|| {
            format!(
                "Missing `[package]` table in Cargo.toml with readme at {}",
                manifest_path.display()
            )
        })?;
        project["readme"] = toml_edit::value(readme_name);
    }
    Ok(())
}

fn rewrite_dependency_paths(
    document: &mut DocumentMut,
    manifest_dir: &Path,
    manifest_dest_base: &Path,
    dep_dest_map: &HashMap<PathBuf, PathBuf>,
) -> Result<bool> {
    let mut changed = false;
    let doc_table = document.as_table_mut();

    changed |=
        rewrite_dependency_tables(doc_table, manifest_dir, manifest_dest_base, dep_dest_map)?;

    if let Some(workspace_item) = doc_table.get_mut("workspace") {
        if let Some(workspace_table) = workspace_item.as_table_mut() {
            changed |= rewrite_dependency_tables(
                workspace_table,
                manifest_dir,
                manifest_dest_base,
                dep_dest_map,
            )?;
        }
    }

    if let Some(target_item) = doc_table.get_mut("target") {
        if let Some(target_table) = target_item.as_table_mut() {
            for (_, item) in target_table.iter_mut() {
                if let Some(target_table) = item.as_table_mut() {
                    changed |= rewrite_dependency_tables(
                        target_table,
                        manifest_dir,
                        manifest_dest_base,
                        dep_dest_map,
                    )?;
                }
            }
        }
    }

    Ok(changed)
}

fn rewrite_dependency_tables(
    table: &mut Table,
    manifest_dir: &Path,
    manifest_dest_base: &Path,
    dep_dest_map: &HashMap<PathBuf, PathBuf>,
) -> Result<bool> {
    let mut changed = false;
    for key in [
        "dependencies",
        "dev-dependencies",
        "build-dependencies",
        "patch",
        "replace",
    ] {
        if let Some(item) = table.get_mut(key) {
            if let Some(dep_table) = item.as_table_mut() {
                changed |= rewrite_dependency_table(
                    dep_table,
                    manifest_dir,
                    manifest_dest_base,
                    dep_dest_map,
                )?;
            }
        }
    }
    Ok(changed)
}

fn rewrite_dependency_table(
    table: &mut Table,
    manifest_dir: &Path,
    manifest_dest_base: &Path,
    dep_dest_map: &HashMap<PathBuf, PathBuf>,
) -> Result<bool> {
    let mut changed = false;
    for (_, item) in table.iter_mut() {
        match item {
            Item::Value(Value::InlineTable(inline)) => {
                if let Some(Value::String(path_value)) = inline.get("path") {
                    if let Some(new_path) = rewrite_path_value(
                        manifest_dir,
                        manifest_dest_base,
                        dep_dest_map,
                        path_value.value(),
                    ) {
                        inline.insert("path", Value::from(new_path));
                        changed = true;
                    }
                }
            }
            Item::Table(dep_table) => {
                if let Some(path_value) = dep_table.get("path").and_then(|v| v.as_value()) {
                    if let Some(path_value) = path_value.as_str() {
                        if let Some(new_path) = rewrite_path_value(
                            manifest_dir,
                            manifest_dest_base,
                            dep_dest_map,
                            path_value,
                        ) {
                            dep_table["path"] = toml_edit::value(new_path);
                            changed = true;
                        }
                    }
                }
            }
            _ => {}
        }
    }
    Ok(changed)
}

fn rewrite_path_value(
    manifest_dir: &Path,
    manifest_dest_base: &Path,
    dep_dest_map: &HashMap<PathBuf, PathBuf>,
    raw_path: &str,
) -> Option<String> {
    let path = Path::new(raw_path);
    let abs_path = if path.is_absolute() {
        normalize_path(path)
    } else {
        normalize_path(&manifest_dir.join(path))
    };
    let dep_dest_base = dep_dest_map.get(&abs_path)?;
    let new_path = relative_path(manifest_dest_base, dep_dest_base);
    Some(new_path.to_string_lossy().to_string())
}

fn common_path(one: &Path, two: &Path) -> Option<PathBuf> {
    let mut final_path = PathBuf::new();
    let mut found = false;
    for (l, r) in one.components().zip(two.components()) {
        if l == r {
            final_path.push(l.as_os_str());
            found = true;
        } else {
            break;
        }
    }
    found.then_some(final_path)
}

fn normalize_path(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::Prefix(prefix) => normalized.push(prefix.as_os_str()),
            std::path::Component::RootDir => normalized.push(component.as_os_str()),
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                normalized.pop();
            }
            std::path::Component::Normal(part) => normalized.push(part),
        }
    }
    normalized
}

fn relative_path(from: &Path, to: &Path) -> PathBuf {
    let from_components: Vec<_> = from.components().collect();
    let to_components: Vec<_> = to.components().collect();
    let mut i = 0;
    while i < from_components.len()
        && i < to_components.len()
        && from_components[i] == to_components[i]
    {
        i += 1;
    }
    let mut out = PathBuf::new();
    for _ in i..from_components.len() {
        out.push("..");
    }
    for comp in &to_components[i..] {
        out.push(comp.as_os_str());
    }
    if out.as_os_str().is_empty() {
        out.push(".");
    }
    out
}
