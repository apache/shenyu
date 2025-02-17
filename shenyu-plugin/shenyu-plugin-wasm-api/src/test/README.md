# How to build the wasm file

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
