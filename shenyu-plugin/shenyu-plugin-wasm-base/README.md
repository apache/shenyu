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

The checked-in binaries' WebAssembly `producers` sections identify Rust 1.95.0
for the plugin, plugin-data and metadata guests, and Rust 1.90.0 for the discovery
guest. Rebuilds may differ byte-for-byte; run the Java tests to verify the rebuilt
artifacts. Runtime copyright and license texts for both versions are included in
the repository's root `licenses` directory and referenced by the root `LICENSE`.

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
The checked-in discovery guest identifies TinyGo 0.43.0; the other TinyGo guests,
including the legacy discovery guest, identify TinyGo 0.41.1. Their runtime,
Go standard-library and LLVM license texts are in
`licenses/LICENSE-wasm-fixtures.txt` at the repository root.

# WASM discovery callbacks

`AbstractWasmDiscoveryHandler` forwards discovery updates to the required guest
export `handlerDiscoveryUpstreamData(argId: i64) -> ()` and deletions to the
optional export `removeDiscoveryUpstreamData(argId: i64) -> ()`.

Both callbacks use the existing Java argument bridge. `getArgumentId` supplies
the ID, and `ARGUMENTS.get(argId)` exposes a `DiscoverySyncData` for the duration
of the callback. A Java subclass can serialize this object from `onGetArgs`,
which serves the guest's `shenyu.get_args` import.

For deletion, the object contains exactly the identity from `DiscoveryUpstreamKey`:

- `pluginName`: the plugin owning the discovery state.
- `selectorId`: the selector to remove.
- `selectorName`: the optional selector name, which can be null.

The remaining fields, including `upstreamDataList`, are null. The guest should
remove the state identified by this key when the deletion callback runs. For a
subclass using JSON serialization, a deletion payload can look like:

```json
{"pluginName":"divide","selectorId":"selector-id","selectorName":"my-selector"}
```

Guests that omit the deletion export receive no callback, preserving the legacy
no-op behavior. Existing Java subclasses do not need a new argument-ID method.
Callback failures propagate to the caller, and the temporary argument is removed
whether the callback succeeds or fails.

The Go and Rust test guests read the deletion payload and echo it through
`shenyu.put_result`. Their build instructions are in `src/test/go-discovery-handler`
and `src/test/rust-discovery-handler`. The `TestLegacyWasmPluginDiscoveryHandler`
fixture preserves the original Go binary without the deletion export to test
compatibility with existing guests.

## CI fixture verification

The `wasm-fixtures` workflow pins WABT 1.0.42 and Rust 1.90.0. It runs
`python3 .github/scripts/verify-wasm-fixtures.py` when either WASM module changes.
The script requires the base module's `wasm-gateway` WAT family and the API
module's `wasm-concurrent` family. It checks every `wasm-*` fixture directory in
both modules byte-for-byte, including `wasm-bridge` when present, without
modifying the committed files. It then compiles the five Rust guests for
`wasm32-wasip1` and runs the WASM Java suites against them. Original resource
bytes are restored even if a build or test fails. The checked-in guests contain
Rust 1.90.0 and 1.95.0 producer records, so Rust reproduction with the CI-pinned
version is checked by behavior rather than exact binary identity.
The existing TinyGo guests run in the same suites. Use `--wat-only` or
`--rust-only` for a scoped local run. Maven-only builds still use committed guests.

Run Rust verification from a clean checkout. It temporarily replaces tracked
`.wasm` resources while Maven tests execute, then restores them on success,
ordinary failure or a Python `KeyboardInterrupt` (Ctrl+C). Forced termination
such as `SIGKILL` bypasses that cleanup; inspect `git diff` and restore only the
affected generated fixtures before committing. Cargo may fetch dependencies;
the verifier does not require a pre-populated offline dependency cache.

The optional `removeDiscoveryUpstreamData` callback documented below is now on
the merged base through #7568. The binary request profile in these gateway tests
belongs to the explicit Java reference adapter; it is not the JSON default
bridge introduced separately by #7569. Both use the same host import signatures
and the merged policy that rejects non-function exports. This documentation PR
keeps `_initialize` validation in the same private lookup helper so a malformed
initializer is not silently treated as an absent optional hook.
