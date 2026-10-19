# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements. See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Rebuild WAT fixtures exactly and test freshly compiled legacy Rust guests."""

import argparse
import shutil
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PLUGIN = ROOT / "shenyu-plugin"
API = PLUGIN / "shenyu-plugin-wasm-api" / "src/test"
BASE = PLUGIN / "shenyu-plugin-wasm-base" / "src/test"
RUST_TOOLCHAIN = "1.90.0"
RUST_GUESTS = [
    (API, "rust-wasm-plugin", "org.apache.shenyu.plugin.wasm.api.AbstractWasmPluginTest$RustWasmPlugin.wasm"),
    (BASE, "rust-shenyu-wasm-plugin", "org.apache.shenyu.plugin.wasm.base.AbstractShenyuWasmPluginTest$TestShenyuWasmPlugin.wasm"),
    (BASE, "rust-plugin-data-handler", "org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmPluginDataHandlerTest$TestWasmPluginDataHandler.wasm"),
    (BASE, "rust-meta-data-handler", "org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmMetaDataHandlerTest$TestWasmMetaDataHandler.wasm"),
    (BASE, "rust-discovery-handler", "org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmDiscoveryHandlerTest$TestWasmPluginDiscoveryHandler.wasm"),
]


def run(command, **kwargs):
    subprocess.run(command, check=True, **kwargs)


def verify_wat():
    for source_root, required in ((BASE, "wasm-gateway"), (API, "wasm-concurrent")):
        if not (source_root / required).is_dir():
            raise RuntimeError("Required WAT fixture family missing: " + required)
    version = subprocess.check_output(["wat2wasm", "--version"], text=True).strip()
    if version != "1.0.42":
        raise RuntimeError("WAT verification requires WABT 1.0.42, found " + version)
    count = 0
    for source_root in (BASE, API):
        for directory in sorted(source_root.glob("wasm-*")):
            if not directory.is_dir():
                raise RuntimeError("WAT fixture family is not a directory: " + directory.name)
            with tempfile.TemporaryDirectory(prefix="shenyu-wat-") as temporary:
                run(["make", "-C", str(directory), "build", "RESOURCES=" + temporary])
                binaries = list(Path(temporary).glob("*.wasm"))
                if not binaries:
                    raise RuntimeError("No fixtures generated for " + directory.name)
                for rebuilt in binaries:
                    committed = source_root / "resources" / rebuilt.name
                    if rebuilt.read_bytes() != committed.read_bytes():
                        raise RuntimeError("WAT fixture differs from committed binary: " + rebuilt.name)
                    count += 1
    if not count:
        raise RuntimeError("No WAT fixture families found")
    print("Verified " + str(count) + " byte-identical WAT fixtures", flush=True)


def verify_rust():
    originals = {}
    try:
        with tempfile.TemporaryDirectory(prefix="shenyu-rust-") as temporary:
            for source_root, crate, resource_name in RUST_GUESTS:
                source = source_root / crate
                copied = Path(temporary) / crate
                copied.mkdir()
                shutil.copyfile(source / "Cargo.toml", copied / "Cargo.toml")
                shutil.copytree(source / "src", copied / "src")
                run(["cargo", "+" + RUST_TOOLCHAIN, "build", "--manifest-path", str(copied / "Cargo.toml"),
                     "--target", "wasm32-wasip1", "--release"])
                binary = copied / "target/wasm32-wasip1/release" / (crate.replace("-", "_") + ".wasm")
                resource = source_root / "resources" / resource_name
                originals[resource] = resource.read_bytes()
                shutil.copyfile(binary, resource)
            # Maven copies the freshly rebuilt guests before the tests load them.
            # Legacy compiler versions were not recorded, so assert behavior instead of bytes.
            run(["./mvnw", "-B", "-ntp", "-pl", "shenyu-plugin/shenyu-plugin-wasm-base", "-am", "test",
                 "-Dtest=*Wasm*Test", "-Dsurefire.failIfNoSpecifiedTests=false",
                 "-DskipRemoteResources=true", "-Djacoco.skip=true", "-Dmaven.javadoc.skip=true"], cwd=ROOT)
    finally:
        for resource, original in originals.items():
            resource.write_bytes(original)
    print("Verified five rebuilt Rust guests through the WASM Java suites", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--wat-only", action="store_true")
    mode.add_argument("--rust-only", action="store_true")
    options = parser.parse_args()
    if not options.rust_only:
        verify_wat()
    if not options.wat_only:
        verify_rust()


if __name__ == "__main__":
    main()
