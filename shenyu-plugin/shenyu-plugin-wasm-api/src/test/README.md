# WASM API fixtures

See the versioned [host ABI v1](../../ABI-v1.md) and the
[real gateway reference test](../../../shenyu-plugin-wasm-base/README.md).
These are fixture builds, not a statement of support for arbitrary Go runtimes.

## Build the Rust fixture

1. install rustup

2. install rust

3. generate the wasm file

```shell
cd {shenyu}/shenyu-plugin/shenyu-plugin-wasm-api/src/test/rust-wasm-plugin
cargo build --target wasm32-wasip1 --release
```

then you will see the wasm file
in `{shenyu}/shenyu-plugin/shenyu-plugin-wasm-api/src/test/rust-wasm-plugin/target/wasm32-wasip1/release/rust_wasm_plugin.wasm`

4. rename the wasm file

rename the file to `org.apache.shenyu.plugin.wasm.api.AbstractWasmPluginTest$RustWasmPlugin.wasm`


From the repository root, the complete Rust build and copy command is:

```sh
rustup target add wasm32-wasip1
api=shenyu-plugin/shenyu-plugin-wasm-api/src/test
cargo build --manifest-path "$api/rust-wasm-plugin/Cargo.toml" --target wasm32-wasip1 --release
cp "$api/rust-wasm-plugin/target/wasm32-wasip1/release/rust_wasm_plugin.wasm" \
  "$api/resources/org.apache.shenyu.plugin.wasm.api.AbstractWasmPluginTest\$RustWasmPlugin.wasm"
```

## Build the TinyGo fixture

```sh
make -C shenyu-plugin/shenyu-plugin-wasm-api/src/test/go-wasm-plugin build
```

This uses TinyGo `-target wasm-unknown -opt=2 -no-debug -panic=trap`. The checked-in
API fixtures identify TinyGo 0.41.1 and Rust 1.95.0 in their WebAssembly
`producers` sections. Runtime copyright and license texts are included in the
repository's root `licenses` directory and referenced by the root `LICENSE`.
Run both module suites using the command in the WASM base README after rebuilding.
