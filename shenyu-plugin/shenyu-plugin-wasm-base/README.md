# WASM base and gateway reference tests

Read the versioned [host ABI v1 and reference profile v1](../shenyu-plugin-wasm-api/ABI-v1.md)
before implementing a guest or Java adapter.

## Run the gateway test

From the repository root, with Java 17:

```sh
./mvnw -B -ntp -pl shenyu-plugin/shenyu-plugin-wasm-base -am test \
  -Dtest=WasmGatewayTest -Dsurefire.failIfNoSpecifiedTests=false \
  -DskipRemoteResources=true -Djacoco.skip=true
```

This starts an embedded gateway on a random TCP port using the gateway starter,
registers the reference plugin and data handler as Spring beans, and sends real
HTTP requests. It verifies selector/rule handles, method, path, query, headers,
UTF-8 body, a guest allow/deny decision, chain continuation/short circuit,
configuration updates/deletes, unmatched requests, 60 concurrent requests,
missing exports, guest traps and invalid memory. Every invocation must release
its Java argument, including failure paths. Failure tests replace the guest with
real compiled Wasm fixtures and then verify healthy requests still succeed.

To run both existing Rust/TinyGo unit suites and the gateway test:

```sh
./mvnw -B -ntp -pl shenyu-plugin/shenyu-plugin-wasm-api,shenyu-plugin/shenyu-plugin-wasm-base \
  -am test -Dtest='*Wasm*Test' -Dsurefire.failIfNoSpecifiedTests=false \
  -DskipRemoteResources=true -Djacoco.skip=true
```

The gateway tests are normal Surefire tests and therefore run in module CI.
Dependencies on Spring Boot and the gateway starter are test-scoped.

## Rebuild the gateway Wasm fixtures

Install WABT 1.0.42 (`wat2wasm --version`). From the repository root:

```sh
make -C shenyu-plugin/shenyu-plugin-wasm-base/src/test/wasm-gateway build
```

The Makefile maps each WAT source to the fully qualified Java resource name,
including quoted `$` characters for nested test classes. The binaries are
checked in so Java builds do not need a Wasm compiler. Rebuilding with the stated
WABT version should leave `git diff` empty for these binaries.

## Rebuild the existing Rust fixtures

Install Rust and the `wasm32-wasip1` target:

```sh
rustup target add wasm32-wasip1
```

From the repository root, build and copy the base fixtures:

```sh
base=shenyu-plugin/shenyu-plugin-wasm-base/src/test
cargo build --manifest-path "$base/rust-shenyu-wasm-plugin/Cargo.toml" --target wasm32-wasip1 --release
cp "$base/rust-shenyu-wasm-plugin/target/wasm32-wasip1/release/rust_shenyu_wasm_plugin.wasm" \
  "$base/resources/org.apache.shenyu.plugin.wasm.base.AbstractShenyuWasmPluginTest\$TestShenyuWasmPlugin.wasm"
cargo build --manifest-path "$base/rust-plugin-data-handler/Cargo.toml" --target wasm32-wasip1 --release
cp "$base/rust-plugin-data-handler/target/wasm32-wasip1/release/rust_plugin_data_handler.wasm" \
  "$base/resources/org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmPluginDataHandlerTest\$TestWasmPluginDataHandler.wasm"
cargo build --manifest-path "$base/rust-meta-data-handler/Cargo.toml" --target wasm32-wasip1 --release
cp "$base/rust-meta-data-handler/target/wasm32-wasip1/release/rust_meta_data_handler.wasm" \
  "$base/resources/org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmMetaDataHandlerTest\$TestWasmMetaDataHandler.wasm"
cargo build --manifest-path "$base/rust-discovery-handler/Cargo.toml" --target wasm32-wasip1 --release
cp "$base/rust-discovery-handler/target/wasm32-wasip1/release/rust_discovery_handler.wasm" \
  "$base/resources/org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmDiscoveryHandlerTest\$TestWasmPluginDiscoveryHandler.wasm"
```

The exact compiler versions of the legacy binaries were not recorded. Rebuilds
may differ byte-for-byte; run the Java tests to verify the rebuilt artifacts.

## Rebuild the existing TinyGo fixtures

With TinyGo installed, from the repository root:

```sh
make -C shenyu-plugin/shenyu-plugin-wasm-base/src/test/go-shenyu-wasm-plugin build
make -C shenyu-plugin/shenyu-plugin-wasm-base/src/test/go-plugin-data-handler build
make -C shenyu-plugin/shenyu-plugin-wasm-base/src/test/go-meta-data-handler build
make -C shenyu-plugin/shenyu-plugin-wasm-base/src/test/go-discovery-handler build
```

These Makefiles build with `-target wasm-unknown -opt=2 -no-debug -panic=trap`
and copy the result to its Java resource name. The test helper and examples are
TinyGo fixtures; they do not certify arbitrary standard Go runtimes.
