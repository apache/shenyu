# How to build the wasm file

1. install tinygo

2. generate the wasm file

Option A (recommended): build and copy with Makefile

```shell
cd {shenyu}/shenyu-plugin/shenyu-plugin-wasm-api/src/test/go-wasm-plugin
make build
```

then you will see the wasm file
in `{shenyu}/shenyu-plugin/shenyu-plugin-wasm-api/src/test/resources/org.apache.shenyu.plugin.wasm.api.AbstractWasmPluginTest$GoWasmPlugin.wasm`

Option B: manual build

```shell
cd {shenyu}/shenyu-plugin/shenyu-plugin-wasm-api/src/test/go-wasm-plugin
tinygo build -target wasm-unknown -opt=2 -no-debug -panic=trap -o plugin.wasm main.go
```

then you will see the wasm file
in `{shenyu}/shenyu-plugin/shenyu-plugin-wasm-api/src/test/go-wasm-plugin/plugin.wasm`

3. rename the wasm file (only for manual build)

rename the file to `org.apache.shenyu.plugin.wasm.api.AbstractWasmPluginTest$GoWasmPlugin.wasm`
