"""Hatch build hook for maturin.

This module provides a Hatch build hook that delegates native artifact
compilation to maturin while using hatchling as the build backend.

Limitations:
- Auditwheel is always skipped (use maturin directly for manylinux compliance)
- WASI binaries are not supported
- Single interpreter builds only

See the module documentation in src/hatch.rs for architecture details.
"""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path
from typing import Any

from hatchling.builders.hooks.plugin.interface import BuildHookInterface
from hatchling.plugin import hookimpl


class MaturinBuildHook(BuildHookInterface):
    """Hatch build hook that compiles native extensions using maturin."""

    PLUGIN_NAME = "maturin"

    def dependencies(self) -> list[str]:
        if self.config.get("bindings") == "cffi":
            return ["cffi"]
        return []

    def initialize(self, version: str, build_data: dict[str, Any]) -> None:
        if self.target_name == "wheel":
            self._build_wheel_artifacts(build_data)
        elif self.target_name == "sdist":
            self._augment_sdist(build_data)

    def _build_wheel_artifacts(self, build_data: dict[str, Any]) -> None:
        output_dir = self._output_dir("wheel")
        args = [
            "maturin",
            "pep517",
            "build-artifacts",
            "--output-dir",
            str(output_dir),
        ]
        args.extend(self._build_args())
        if self.config.get("strip"):
            args.append("--strip")

        data = self._run(args)
        build_data["tag"] = data["tag"]
        build_data["pure_python"] = False
        build_data.setdefault("force_include", {}).update(data.get("force_include", {}))

    def _augment_sdist(self, build_data: dict[str, Any]) -> None:
        output_dir = self._output_dir("sdist")
        args = [
            "maturin",
            "pep517",
            "sdist-augment",
            "--output-dir",
            str(output_dir),
        ]
        args.extend(self._build_args())

        data = self._run(args)
        build_data.setdefault("force_include", {}).update(data.get("force_include", {}))

    def _build_args(self) -> list[str]:
        args: list[str] = []
        config = self.config

        manifest_path = config.get("manifest-path")
        if manifest_path:
            args.extend(["--manifest-path", str(manifest_path)])

        bindings = config.get("bindings")
        if bindings:
            args.extend(["--bindings", str(bindings)])

        features = config.get("features")
        if features:
            for feature in self._as_list(features):
                args.extend(["--features", feature])

        profile = config.get("profile")
        if profile:
            args.extend(["--profile", str(profile)])
        elif config.get("release"):
            args.extend(["--profile", "release"])

        compatibility = config.get("compatibility")
        if compatibility:
            for tag in self._as_list(compatibility):
                args.extend(["--compatibility", tag])

        interpreter = config.get("interpreter")
        if interpreter:
            for item in self._as_list(interpreter):
                args.extend(["--interpreter", item])

        target = config.get("target")
        if target:
            args.extend(["--target", str(target)])

        if config.get("zig"):
            args.append("--zig")

        return args

    def _output_dir(self, target: str) -> Path:
        output_dir = Path(self.directory) / ".maturin" / target
        output_dir.mkdir(parents=True, exist_ok=True)
        return output_dir

    def _run(self, args: list[str]) -> dict[str, Any]:
        result = subprocess.run(
            args,
            cwd=self.root,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        if result.returncode != 0:
            # Print stderr so users see compiler output
            sys.stderr.write(result.stderr)
            raise RuntimeError(f"maturin build failed (exit code {result.returncode})\ncommand: {' '.join(args)}")
        output = result.stdout.strip()
        if not output:
            raise RuntimeError("maturin hook did not return any output")
        return json.loads(output.splitlines()[-1])

    @staticmethod
    def _as_list(value: str | list[str]) -> list[str]:
        if isinstance(value, str):
            return value.split(",")
        return list(value)


@hookimpl
def hatch_register_build_hook() -> type[MaturinBuildHook]:
    return MaturinBuildHook
