use anyhow::{Context, Result, bail};
use clap::Parser;
use expect_test::Expect;
use flate2::read::GzDecoder;
use fs_err::File;
use maturin::pyproject_toml::{SdistGenerator, ToolMaturin};
#[cfg(feature = "hatch")]
use maturin::sdist_augment;
use maturin::{BuildOptions, CargoOptions, PlatformTag};
use pretty_assertions::assert_eq;
#[cfg(feature = "hatch")]
use std::collections::BTreeSet;
#[cfg(feature = "hatch")]
use std::env;
use std::io::Read;
use std::path::{Path, PathBuf};
#[cfg(feature = "hatch")]
use std::process::Command;
use tar::Archive;
use time::OffsetDateTime;
use zip::ZipArchive;

#[cfg(feature = "hatch")]
use crate::common::create_virtualenv;

fn copy_dir_recursive(src: &Path, dst: &Path) -> std::io::Result<()> {
    fs_err::create_dir_all(dst)?;
    for entry in fs_err::read_dir(src)? {
        let entry = entry?;
        let name = entry.file_name();
        // Skip build artifacts and caches
        if name.to_str() == Some("target") {
            continue;
        }
        let src_path = entry.path();
        let dst_path = dst.join(name);
        if src_path.is_dir() {
            copy_dir_recursive(&src_path, &dst_path)?;
        } else {
            fs_err::copy(&src_path, &dst_path)?;
        }
    }
    Ok(())
}

/// Tries to compile a sample crate (pyo3-pure) for musl,
/// given that rustup and the the musl target are installed
///
/// The bool in the Ok() response says whether the test was actually run
pub fn test_musl() -> Result<bool> {
    use anyhow::bail;
    use fs_err as fs;
    use fs_err::File;
    use goblin::elf::Elf;
    use std::io::ErrorKind;
    use std::process::Command;

    let get_target_list = Command::new("rustup")
        .args(["target", "list", "--installed"])
        .output();

    match get_target_list {
        Ok(output) => {
            if output.status.success() {
                let has_musl = String::from_utf8_lossy(&output.stdout)
                    .lines()
                    .any(|line| line.trim() == "x86_64-unknown-linux-musl");
                if !has_musl {
                    return Ok(false);
                }
            } else {
                bail!(
                    "`rustup target list --installed` failed with status {}",
                    output.status
                )
            }
        }
        // Ignore installations without rustup
        Err(err) if err.kind() == ErrorKind::NotFound => return Ok(false),
        Err(err) => return Err(err.into()),
    };

    // The first arg gets ignored
    let options: BuildOptions = BuildOptions::try_parse_from([
        "build",
        "--manifest-path",
        "test-crates/hello-world/Cargo.toml",
        "--interpreter",
        "python3",
        "--target",
        "x86_64-unknown-linux-musl",
        "--compatibility",
        "linux",
        "--quiet",
        "--target-dir",
        "test-crates/targets/test_musl",
        "--out",
        "test-crates/wheels/test_musl",
    ])?;

    let build_context = options
        .into_build_context()
        .strip(cfg!(feature = "faster-tests"))
        .editable(false)
        .build()?;
    let built_lib =
        PathBuf::from("test-crates/targets/test_musl/x86_64-unknown-linux-musl/debug/hello-world");
    if built_lib.is_file() {
        fs::remove_file(&built_lib)?;
    }
    let wheels = build_context.build_wheels()?;
    assert_eq!(wheels.len(), 1);

    // Ensure that we've actually built for musl
    assert!(built_lib.is_file());
    let mut file = File::open(built_lib)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    let elf = Elf::parse(&buffer)?;
    assert_eq!(elf.libraries, Vec::<&str>::new());

    Ok(true)
}

/// Test that we ignore non-existent Cargo.lock file listed by `cargo package --list`,
/// which seems to only occur with workspaces.
/// See https://github.com/rust-lang/cargo/issues/7938#issuecomment-593280660 and
/// https://github.com/PyO3/maturin/issues/449
pub fn test_workspace_cargo_lock() -> Result<()> {
    // The first arg gets ignored
    let options: BuildOptions = BuildOptions::try_parse_from([
        "build",
        "--manifest-path",
        "test-crates/workspace/py/Cargo.toml",
        "--compatibility",
        "linux",
        "--quiet",
        "--target-dir",
        "test-crates/targets/test_workspace_cargo_lock",
        "--out",
        "test-crates/wheels/test_workspace_cargo_lock",
    ])?;

    let build_context = options
        .into_build_context()
        .strip(false)
        .editable(false)
        .build()?;
    let source_distribution = build_context.build_source_distribution()?;
    assert!(source_distribution.is_some());

    Ok(())
}

pub fn build_source_distribution(
    package: impl AsRef<Path>,
    sdist_generator: SdistGenerator,
    unique_name: &str,
) -> Result<Archive<GzDecoder<File>>> {
    let manifest_path = package.as_ref().join("Cargo.toml");
    let sdist_directory = Path::new("test-crates").join("wheels").join(unique_name);

    let build_options = BuildOptions {
        out: Some(sdist_directory),
        cargo: CargoOptions {
            manifest_path: Some(manifest_path),
            quiet: true,
            target_dir: Some(PathBuf::from(
                "test-crates/targets/test_workspace_cargo_lock",
            )),
            ..Default::default()
        },
        ..Default::default()
    };

    let mut build_context = build_options
        .into_build_context()
        .strip(false)
        .editable(false)
        .sdist_only(true)
        .build()?;

    // Override the sdist generator for testing
    let mut pyproject_toml = build_context.pyproject_toml.take().unwrap();
    let mut tool = pyproject_toml.tool.clone().unwrap_or_default();
    if let Some(ref mut tool_maturin) = tool.maturin {
        tool_maturin.sdist_generator = sdist_generator;
    } else {
        tool.maturin = Some(ToolMaturin {
            sdist_generator,
            ..Default::default()
        });
    }
    pyproject_toml.tool = Some(tool);
    build_context.pyproject_toml = Some(pyproject_toml);

    let (path, _) = build_context
        .build_source_distribution()?
        .context("Failed to build source distribution")?;

    let tar_gz = fs_err::File::open(path)?;
    let tar = GzDecoder::new(tar_gz);
    let archive = Archive::new(tar);
    Ok(archive)
}

pub fn test_source_distribution(
    package: impl AsRef<Path>,
    sdist_generator: SdistGenerator,
    expected_files: Expect,
    expected_cargo_toml: Option<(&Path, Expect)>,
    unique_name: &str,
) -> Result<()> {
    let mut archive = build_source_distribution(package, sdist_generator, unique_name)?;
    let mut files = BTreeSet::new();
    let mut file_count = 0;
    let mut cargo_toml = None;
    for entry in archive.entries()? {
        let mut entry = entry?;
        files.insert(format!("{}", entry.path()?.display()));
        file_count += 1;
        if let Some(cargo_toml_path) = expected_cargo_toml.as_ref().map(|(p, _)| *p) {
            if entry.path()? == cargo_toml_path {
                let mut contents = String::new();
                entry.read_to_string(&mut contents)?;
                cargo_toml = Some(contents);
            }
        }
    }
    expected_files.assert_debug_eq(&files);
    assert_eq!(
        file_count,
        files.len(),
        "duplicated files found in sdist: {:?}",
        files
    );

    if let Some((cargo_toml_path, expected)) = expected_cargo_toml {
        let cargo_toml = cargo_toml
            .with_context(|| format!("{} not found in sdist", cargo_toml_path.display()))?;
        expected.assert_eq(&cargo_toml.replace("\r\n", "\n"));
    }
    Ok(())
}

pub fn check_sdist_mtimes(
    package: impl AsRef<Path>,
    expected_mtime: u64,
    unique_name: &str,
) -> Result<()> {
    let mut archive = build_source_distribution(package, SdistGenerator::Cargo, unique_name)?;

    for entry in archive.entries()? {
        let entry = entry?;
        let filename = entry.header().path()?;
        let mtime = entry.header().mtime()?;

        assert_eq!(
            mtime,
            expected_mtime,
            "File {} has an mtime of {} instead of {}",
            filename.display(),
            mtime,
            expected_mtime
        );
    }

    Ok(())
}

fn build_wheel_files(package: impl AsRef<Path>, unique_name: &str) -> Result<ZipArchive<File>> {
    let manifest_path = package.as_ref().join("Cargo.toml");
    let wheel_directory = Path::new("test-crates").join("wheels").join(unique_name);

    let build_options = BuildOptions {
        out: Some(wheel_directory),
        cargo: CargoOptions {
            manifest_path: Some(manifest_path),
            quiet: true,
            target_dir: Some(PathBuf::from(format!("test-crates/targets/{unique_name}"))),
            ..Default::default()
        },
        platform_tag: vec![PlatformTag::Linux],
        ..Default::default()
    };

    let build_context = build_options
        .into_build_context()
        .strip(false)
        .editable(false)
        .build()?;
    let wheels = build_context
        .build_wheels()
        .context("Failed to build wheels")?;
    assert!(!wheels.is_empty());
    let (wheel_path, _) = &wheels[0];

    let wheel = ZipArchive::new(File::open(wheel_path)?)?;
    Ok(wheel)
}

pub fn check_wheel_mtimes(
    package: impl AsRef<Path>,
    expected_mtime: Vec<OffsetDateTime>,
    unique_name: &str,
) -> Result<()> {
    let mut wheel = build_wheel_files(package, unique_name)?;
    let mut mtimes = BTreeSet::<OffsetDateTime>::new();

    for idx in 0..wheel.len() {
        let mtime = wheel.by_index(idx)?.last_modified().unwrap().try_into()?;
        mtimes.insert(mtime);
    }

    assert_eq!(mtimes, expected_mtime.into_iter().collect::<BTreeSet<_>>());

    Ok(())
}

pub fn check_wheel_files(
    package: impl AsRef<Path>,
    expected_files: Vec<&str>,
    unique_name: &str,
) -> Result<()> {
    let wheel = build_wheel_files(package, unique_name)?;
    let drop_platform_specific_files = |file: &&str| -> bool {
        !matches!(Path::new(file).extension(), Some(ext) if ext == "pyc" || ext == "pyd" || ext == "so")
    };
    assert_eq!(
        wheel
            .file_names()
            .filter(drop_platform_specific_files)
            .collect::<BTreeSet<_>>(),
        expected_files.into_iter().collect::<BTreeSet<_>>()
    );
    Ok(())
}

pub fn abi3_python_interpreter_args() -> Result<()> {
    // Case 1: maturin build without `-i`, should work
    let options = BuildOptions::try_parse_from(vec![
        "build",
        "--manifest-path",
        "test-crates/pyo3-pure/Cargo.toml",
        "--quiet",
    ])?;
    let result = options
        .into_build_context()
        .strip(cfg!(feature = "faster-tests"))
        .editable(false)
        .build();
    assert!(result.is_ok());

    // Case 2: maturin build -i python3.10, should work because python3.10 is in bundled sysconfigs
    let options = BuildOptions::try_parse_from(vec![
        "build",
        "--manifest-path",
        "test-crates/pyo3-pure/Cargo.toml",
        "--quiet",
        "-i",
        "python3.10",
    ])?;
    let result = options
        .into_build_context()
        .strip(cfg!(feature = "faster-tests"))
        .editable(false)
        .build();
    assert!(result.is_ok());

    // Windows is a bit different so we exclude it from case 3 & 4

    // Case 3: maturin build -i python2.7, errors because python2.7 is supported
    #[cfg(not(windows))]
    {
        let options = BuildOptions::try_parse_from(vec![
            "build",
            "--manifest-path",
            "test-crates/pyo3-pure/Cargo.toml",
            "--quiet",
            "-i",
            "python2.7",
        ])?;
        let result = options
            .into_build_context()
            .strip(cfg!(feature = "faster-tests"))
            .editable(false)
            .build();
        assert!(result.is_err());

        // Case 4: maturin build -i python-does-not-exists, errors because python executable is not found
        let options = BuildOptions::try_parse_from(vec![
            "build",
            "--manifest-path",
            "test-crates/pyo3-pure/Cargo.toml",
            "--quiet",
            "-i",
            "python-does-not-exists",
        ])?;
        let result = options
            .into_build_context()
            .strip(cfg!(feature = "faster-tests"))
            .editable(false)
            .build();
        assert!(result.is_err());
    }

    Ok(())
}

pub fn abi3_without_version() -> Result<()> {
    // The first argument is ignored by clap
    let cli = vec![
        "build",
        "--manifest-path",
        "test-crates/pyo3-abi3-without-version/Cargo.toml",
        "--quiet",
        "--interpreter",
        "python3",
        "--target-dir",
        "test-targets/wheels/abi3_without_version",
    ];

    let options = BuildOptions::try_parse_from(cli)?;
    let result = options
        .into_build_context()
        .strip(cfg!(feature = "faster-tests"))
        .editable(false)
        .build();
    assert!(result.is_ok());

    Ok(())
}

/// Test that builds succeed even when there are unreadable directories in the project root.
///
/// See https://github.com/PyO3/maturin/issues/2777
#[cfg(unix)]
pub fn test_unreadable_dir() -> Result<()> {
    use std::os::unix::fs::PermissionsExt;

    // Root bypasses permission checks, so we can't test this as root.
    if nix::unistd::getuid().is_root() {
        return Ok(());
    }

    let temp_dir = tempfile::tempdir()?;
    let project_dir = temp_dir.path().join("pyo3-mixed");
    copy_dir_recursive(Path::new("test-crates/pyo3-mixed"), &project_dir)?;

    // Create an unreadable dir that is not the python package.
    let unreadable_dir = project_dir.join("unreadable_cache");
    fs_err::create_dir(&unreadable_dir)?;
    fs_err::write(unreadable_dir.join("cache_file"), "cached data")?;
    fs_err::set_permissions(&unreadable_dir, std::fs::Permissions::from_mode(0o000))?;
    assert!(
        fs_err::read_dir(&unreadable_dir).is_err(),
        "Directory must be unreadable"
    );

    // Test source dist build. See also https://github.com/rust-lang/cargo/issues/16465
    let sdist_options = BuildOptions::try_parse_from([
        "build",
        "--manifest-path",
        project_dir.join("Cargo.toml").to_str().unwrap(),
        "--quiet",
        "--target-dir",
        temp_dir.path().join("target").to_str().unwrap(),
        "--out",
        temp_dir.path().join("dist").to_str().unwrap(),
    ])?;

    let sdist_context = sdist_options
        .into_build_context()
        .strip(false)
        .editable(false)
        .sdist_only(true)
        .build()?;
    sdist_context.build_source_distribution()?;

    // Test wheel build
    let wheel_options = BuildOptions::try_parse_from([
        "build",
        "--manifest-path",
        project_dir.join("Cargo.toml").to_str().unwrap(),
        "--quiet",
        "--target-dir",
        temp_dir.path().join("target").to_str().unwrap(),
        "--out",
        temp_dir.path().join("dist").to_str().unwrap(),
    ])?;

    let wheel_context = wheel_options
        .into_build_context()
        .strip(cfg!(feature = "faster-tests"))
        .editable(false)
        .build()?;
    let wheel_result = wheel_context.build_wheels();

    // Restore permissions before temp_dir cleanup
    fs_err::set_permissions(&unreadable_dir, std::fs::Permissions::from_mode(0o755))?;

    wheel_result?;
    Ok(())
}

#[cfg(feature = "hatch")]
pub fn test_sdist_augment_path_deps() -> Result<()> {
    let output_dir = PathBuf::from("test-crates/targets/pep517_sdist_augment");
    if output_dir.exists() {
        fs_err::remove_dir_all(&output_dir)?;
    }

    let build_options = BuildOptions {
        cargo: CargoOptions {
            manifest_path: Some(PathBuf::from("test-crates/sdist_with_path_dep/Cargo.toml")),
            quiet: true,
            target_dir: Some(PathBuf::from(
                "test-crates/targets/pep517_sdist_augment_target",
            )),
            ..Default::default()
        },
        ..Default::default()
    };

    let output = sdist_augment(build_options, &output_dir)?;
    let targets = output
        .force_include
        .values()
        .cloned()
        .collect::<BTreeSet<_>>();

    for expected in [
        "sdist_with_path_dep/Cargo.toml",
        "some_path_dep/Cargo.toml",
        "some_path_dep/src/lib.rs",
        "transitive_path_dep/Cargo.toml",
        "transitive_path_dep/src/lib.rs",
    ] {
        assert!(
            targets.contains(expected),
            "missing {expected} in sdist augment targets: {targets:?}"
        );
    }

    Ok(())
}

#[cfg(feature = "hatch")]
pub fn test_hatch_build_hook() -> Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let project_dir = temp_dir.path().join("pyo3-mixed");
    copy_dir_recursive(Path::new("test-crates/pyo3-mixed"), &project_dir)?;
    remove_prebuilt_binaries(&project_dir)?;

    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR")).canonicalize()?;
    let repo_url = file_url(&repo_root);
    let pyproject = format!(
        r#"[build-system]
requires = ["hatchling", "maturin @ {repo_url}"]
build-backend = "hatchling.build"

[project]
name = "pyo3-mixed"
version = "2.1.5"
classifiers = ["Programming Language :: Python", "Programming Language :: Rust"]
requires-python = ">=3.7"
dependencies = ["boltons"]
license = "MIT"

[project.scripts]
get_42 = "pyo3_mixed:get_42"
print_cli_args = "pyo3_mixed:print_cli_args"

[tool.hatch.build.targets.wheel]
packages = ["pyo3_mixed"]

[tool.hatch.build.hooks.maturin]
bindings = "pyo3"
"#,
    );
    fs_err::write(project_dir.join("pyproject.toml"), pyproject)?;

    let unique_name = temp_dir
        .path()
        .file_name()
        .map(|name| name.to_string_lossy().to_string())
        .unwrap_or_else(|| "hatch-build-hook".to_string());
    let (_venv_dir, python) = create_virtualenv(&format!("hatch-build-{unique_name}"), None)?;

    let output = Command::new(&python)
        .args([
            "-m",
            "pip",
            "--disable-pip-version-check",
            "--no-cache-dir",
            "install",
            "hatch",
            "hatchling",
        ])
        .output()
        .context("Failed to install hatch into virtualenv")?;
    if !output.status.success() {
        bail!(
            "Failed to install hatch: {}\n---stdout:\n{}\n---stderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let python_dir = python
        .parent()
        .context("Missing virtualenv bin directory")?;
    let old_path = env::var_os("PATH").unwrap_or_default();
    let mut paths = env::split_paths(&old_path).collect::<Vec<_>>();
    paths.insert(0, python_dir.to_path_buf());
    let new_path = env::join_paths(paths).context("Failed to update PATH for hatch")?;

    let hatch_data_dir = temp_dir.path().join("hatch-data");
    let hatch_cache_dir = temp_dir.path().join("hatch-cache");
    let output = Command::new("hatch")
        .current_dir(&project_dir)
        .args(["build", "-t", "sdist", "-t", "wheel"])
        .env("PATH", new_path)
        .env("HATCH_DATA_DIR", &hatch_data_dir)
        .env("HATCH_CACHE_DIR", &hatch_cache_dir)
        .output()
        .context("Failed to invoke hatch build")?;
    if !output.status.success() {
        bail!(
            "Hatch build failed: {}\n---stdout:\n{}\n---stderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let dist_dir = project_dir.join("dist");
    let mut wheel_path = None;
    let mut sdist_path = None;
    for entry in fs_err::read_dir(&dist_dir)? {
        let entry = entry?;
        let path = entry.path();
        if let Some(name) = path.file_name().and_then(|name| name.to_str()) {
            if name.ends_with(".whl") {
                wheel_path = Some(path);
            } else if name.ends_with(".tar.gz") {
                sdist_path = Some(path);
            }
        }
    }

    let wheel_path = wheel_path.context("Missing wheel output from hatch build")?;
    let _sdist_path = sdist_path.context("Missing sdist output from hatch build")?;

    let mut wheel = ZipArchive::new(File::open(&wheel_path)?)?;
    let has_extension = (0..wheel.len()).any(|idx| {
        wheel.by_index(idx).map_or(false, |entry| {
            let name = entry.name();
            name.contains("pyo3_mixed")
                && matches!(
                    Path::new(name).extension().and_then(|e| e.to_str()),
                    Some("so" | "pyd" | "dll" | "dylib")
                )
        })
    });
    assert!(has_extension, "Wheel missing extension module");

    Ok(())
}

#[cfg(feature = "hatch")]
fn remove_prebuilt_binaries(root: &Path) -> Result<()> {
    for entry in fs_err::read_dir(root)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            remove_prebuilt_binaries(&path)?;
        } else if matches!(
            path.extension().and_then(|ext| ext.to_str()),
            Some("so" | "pyd" | "dll" | "dylib")
        ) {
            fs_err::remove_file(&path)?;
        }
    }
    Ok(())
}

#[cfg(feature = "hatch")]
fn file_url(path: &Path) -> String {
    url::Url::from_file_path(path).unwrap().to_string()
}
