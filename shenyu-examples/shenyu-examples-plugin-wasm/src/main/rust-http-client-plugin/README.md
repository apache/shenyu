# How to build the wasm file

1. install rustup

2. install rust

3. generate the wasm file

```shell
cd {shenyu}/shenyu-examples/shenyu-examples-plugin-wasm/src/main/rust-http-client-plugin
cargo build --target wasm32-wasi --release
```

then you will see the wasm file
in `{shenyu}/shenyu-examples/shenyu-examples-plugin-wasm/src/main/rust-http-client-plugin/target/wasm32-wasi/release/rust_http_client_plugin.wasm`

4. rename the wasm file

rename the file to `org.apache.shenyu.examples.plugin.wasm.RustHttpClientPlugin.wasm`
