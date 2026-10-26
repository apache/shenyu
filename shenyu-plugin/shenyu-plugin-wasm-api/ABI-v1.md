# ShenYu WASM host ABI, version 1

This is the versioned contract for `shenyu-plugin-wasm-api` and
`shenyu-plugin-wasm-base` in ShenYu `2.7.2-SNAPSHOT` with Java 17 and Chicory
1.7.3. **Host ABI v1** names the existing transport and lifecycle contract;
**reference request profile v1** below names one concrete serialization and
error-code policy. They are separate contracts: the abstract runtime does not
serialize HTTP exchanges or configuration DTOs automatically.

## Loading and initialization

A Java subclass is still needed to choose the plugin name, order, argument IDs,
serialization, host callbacks and chain behavior. There is no standalone starter
for these abstract libraries. Register a concrete `ShenyuPlugin` bean with the
gateway; register its `PluginDataHandler` bean if configuration hooks are needed.

The resource name is the concrete Java class's fully qualified binary name plus
`.wasm`, at the classpath root. For a nested class the name contains `$`, not a
package directory or a dot between enclosing and nested names. Each loader owns
one Chicory instance and its linear memory. It tries the runtime compiler first
and falls back to the interpreter when compilation/instantiation fails.

The loader registers `wasi_snapshot_preview1` imports, then the `shenyu` imports,
and instantiates with automatic start disabled. If `_initialize: () -> ()` is
exported, it is called once during construction (needed by the TinyGo fixtures).
Neither `_start` nor `main` is called. Linear memory must be available. An absent
resource or unavailable memory prevents initialization; malformed modules,
unresolved imports, invalid export types and initialization traps propagate as
initialization failures rather than being treated as missing optional hooks.

WASI stdout/stderr are attached to the Java process. Environment, arguments and
filesystem access are not inherited. The loader overrides `proc_exit(i32)`:
zero returns normally, while nonzero throws `ShenyuWasmException`. Returning from
this import is ShenYu-specific; guests must not assume it terminates execution.

## Imports and memory ownership

The namespace is exactly `shenyu` (case sensitive). Both imports have this Wasm
signature, including the **i64 pointer** even on wasm32:

```wat
(import "shenyu" "get_args"   (func (param i64 i64 i32) (result i32)))
(import "shenyu" "put_result" (func (param i64 i64 i32) (result i32)))
```

| Parameter | Meaning |
| --- | --- |
| `argId: i64` | Opaque Java-side invocation ID; not an object address. |
| `addr: i64` | Byte offset into the calling instance's linear memory. wasm32 guests widen their pointer to i64. |
| `len: i32` | Buffer capacity for `get_args`, exact payload byte count for `put_result`. |

`get_args` lets the host write serialized arguments into guest-owned memory.
Its conventional success result is the number of bytes written; zero means no
data. `put_result` lets the host copy/process bytes from guest-owned memory;
its conventional success result is zero, with nonzero indicating an adapter
error. The guest must keep the referenced bytes valid until the import returns.
The host must copy any bytes it needs after the callback. Lengths count bytes,
not characters; strings need an explicitly agreed encoding and are not
implicitly NUL-terminated.

Validate nonnegative lengths and the complete `addr..addr+len` range before
accessing memory. Do not silently truncate i64 addresses to Java int values.
The reference adapter uses `Math.toIntExact` and Chicory's checked memory access;
it does not support offsets beyond Java's signed int range. Address zero is a
valid memory offset, not a null pointer by itself. A zero-length payload needs
an adapter policy; the runtime does not impose one.

### Serialization, short buffers and codes

`WasmLoader`'s built-in callbacks are **no-ops returning zero**. Override
`onGetArgs`/`onPutResult` (for loader subclasses), or register imports with the
`Consumer<Store>` constructor of `AbstractShenyuWasmPlugin`. A non-null consumer
replaces both built-in `shenyu` imports and skips `initWasmCallJavaFunc`; it does
not replace the WASI imports. Host callbacks can be registered during
construction, so they must not require subclass fields before those fields have
been initialized.

Host ABI v1 has no universal JSON schema, short-buffer retry protocol or error
number registry. Existing Rust/TinyGo unit adapters use small UTF-8 strings and
may copy `min(capacity, payload size)`. Their behavior does not establish a
lossless production protocol. Every concrete adapter must document its payload,
short-buffer behavior, return codes and result validation. The reference profile
below specifies and tests all of these. Negative codes are ordinary i32 import
results; the loader does not automatically turn them into exceptions or HTTP
responses. A guest must check them. Likewise, Java callers do not interpret a
lifecycle export's return value as a chain decision: use `put_result` and the
Java continuation method.

## Lifecycle exports

All names are case sensitive. The fixtures use `(i64) -> ()` for ID-bearing
exports and `() -> ()` for `refresh` and `_initialize`. Extra return values are
not interpreted by the templates; adapters should use the documented signatures.

| Java abstraction | Export | Required when invoked | Missing-export behavior |
| --- | --- | --- | --- |
| `AbstractWasmPlugin` | `execute(i64)` | Yes | HTTP 500, ShenYu result code 533 (`WASM_FUNC_NOT_FOUND`); no continuation. |
| `AbstractShenyuWasmPlugin` | `doExecute(i64)` | Yes, after selector/rule matching | Same HTTP 500/result code 533; no continuation. |
| Either request plugin | `before(i64)`, `after(i64)` | No | No-op. |
| `AbstractWasmPluginDataHandler` | `handlerPlugin(i64)`, `removePlugin(i64)` | No | No-op. |
| Same data handler | `handlerSelector(i64)`, `removeSelector(i64)` | No | No-op. |
| Same data handler | `handlerRule(i64)`, `removeRule(i64)` | No | No-op. |
| `AbstractWasmMetaDataHandler` | `handleMetaData(i64)`, `removeMetaData(i64)` | Yes, for the corresponding operation | `ShenyuWasmInitException`. |
| Same metadata handler | `refresh()` | No | No-op. |
| `AbstractWasmDiscoveryHandler` | `handlerDiscoveryUpstreamData(i64)` | Yes | `ShenyuWasmInitException`. |
| Any loader | `_initialize()` | No | No-op during loading. |

An export with a matching name but the wrong kind (for example a global instead
of a function) is invalid, not an absent optional hook.

Request invocation arguments include the exchange and chain. The matched base
plugin also supplies `SelectorData` and `RuleData`. `before`/`after` receive an
exchange with null chain/selector/rule. Plugin data hooks receive their DTO;
metadata/discovery hooks receive their corresponding DTO. `refresh` has no
invocation ID and does not populate an argument map. In particular, the legacy
metadata fixture's use of ID zero during refresh is its own adapter convention.

Java publishes an argument immediately before entering the guest and removes it
in `finally`, including on traps or callback failures. It has already been
removed when the Java `doExecute(..., argumentId)` continuation runs. Copy the
result into request-owned storage during `put_result`; retaining just the ID
cannot retrieve the argument later. IDs must be unique across concurrent
invocations sharing the static argument maps, including plugin instances.
The default plugin-data ID methods return zero and must be overridden for
concurrent use.

The gateway's `before`/`after` calls surround construction of the plugin's
reactive publisher, not completion of the asynchronous HTTP response. Guests
must not use `after` as an asynchronous response-completion hook.

A concurrent argument map does not make a Chicory instance or its linear memory
safe for overlapping calls. Serialize access to a shared instance, including
its callbacks and result copying, or use isolated instances. The reference
plugin serializes synchronous guest execution and stores its copied decision on
the exchange; its data handler serializes all six configuration operations.
The abstract libraries do not provide scheduling, pooling, execution deadlines
or automatic retries.

## Reference request profile v1

The [gateway reference test](../shenyu-plugin-wasm-base/src/test/java/org/apache/shenyu/plugin/wasm/base/gateway/WasmGatewayTest.java)
bootstraps a Spring Boot WebFlux server with the same
`shenyu-spring-boot-starter-gateway` used by `shenyu-bootstrap`. Its concrete
[Java plugin](../shenyu-plugin-wasm-base/src/test/java/org/apache/shenyu/plugin/wasm/base/gateway/ReferenceWasmPlugin.java)
and [configuration handler](../shenyu-plugin-wasm-base/src/test/java/org/apache/shenyu/plugin/wasm/base/gateway/ReferenceWasmPluginDataHandler.java)
are Spring beans discovered by the real gateway configuration. HTTP requests
travel through `ShenyuWebHandler`; configuration enters through the real
`PluginDataSubscriber`, matching cache and guest configuration hooks.

The [WAT guest](../shenyu-plugin-wasm-base/src/test/wasm-gateway/reference-plugin.wat)
allows a request when `X-Token` equals the rule handle **and** the selector handle
is a prefix of the request path. The guest decides; Java translates the copied
decision to either `chain.execute(exchange)` or HTTP 403. A terminal Java plugin
adds `X-Chain-Reached: true` and responds with the guest's echoed fields. Denied
requests have no chain header. Unmatched/disabled/deleted configurations bypass
the WASM execution and continue normally. This example is a test reference,
not a production authentication plugin.

### Request frame

All integers are four-byte little-endian values. The frame starts with version
`1`, followed by ten fields. Each field is `byteLength: i32` followed by that
many UTF-8 bytes, with no terminator or padding:

1. selector ID
2. selector handle (path prefix)
3. rule ID
4. rule handle (accepted token)
5. HTTP method
6. raw request path
7. raw query, or empty string
8. `X-Token`, or empty string
9. `X-Request-Id`, or empty string
10. UTF-8 request body, or empty string

The guest owns one 64-KiB memory page. It reads the frame at offset 16 and leaves
four bytes at offset 12 for its decision. The adapter buffers at most 32 KiB of
HTTP body for this example; a production implementation must specify its own
body limits, replay and streaming behavior. The terminal example consumes the
copied frame and does not forward the body to an upstream service.

| Import outcome | Reference code | Behavior |
| --- | --- | --- |
| `get_args` success | Positive payload byte count | Full frame written. |
| No argument for this ID | `-2` | No memory write. |
| Buffer too small | `-1` | No partial write; guest retries with its full 65520-byte buffer. |
| `put_result` success | `0` | Copy and decode result onto the exchange. |
| `put_result` unknown ID | `-2` | No result accepted. |
| Invalid memory, version, decision or frame | Exception/trap | Request fails; no chain continuation; argument removed. |

The guest deliberately starts with capacity one and requires `-1` before
retrying. A frame still too large for the full buffer fails the guest invocation;
there is no dynamic memory growth or truncation. The result is a decision i32
(`0` allow, `1` deny) followed by the original request frame. Version and field
lengths are validated by the Java decoder; malformed results fail the request.

### Configuration frame

The handler's `get_args` returns the complete UTF-8 Gson serialization of the
`PluginData`, `SelectorData` or `RuleData` DTO, with the same `-1`/`-2` policy.
The configuration guest acknowledges the unchanged JSON through `put_result`,
prefixed with an i32 event: `0` plugin update, `1` selector update, `2` rule update,
`3` plugin delete, `4` selector delete, `5` rule delete. Tests compare the actual
guest acknowledgement with the sent DTO and then verify subsequent HTTP behavior.
The handler and request guest are separate instances; the Java matching cache
delivers the latest matched handles to the request guest. This profile does not
promise stable DTO JSON across future ShenYu versions.

## Compatibility and tested toolchains

Host ABI v1 is documented, not negotiated on the wire: there is no automatic
`abi_version` import/export or loader rejection based on this document's version.
An adapter must select compatible guests. Changing import types/names, pointer
widths, required exports or payload/error semantics requires a new version of
the affected contract; optional exports may be added without making older guests
implement them. A plugin-specific payload change needs a profile version change,
not necessarily a new host ABI. The reference frame's version is checked by both
sides, so it must stay at 1 unless both adapter and guest are updated.

| Fixture family | Target/build | What tests establish |
| --- | --- | --- |
| Existing Rust fixtures | Rust edition 2021 `cdylib`, `cargo build --target wasm32-wasip1 --release` | WASI preview1, imports, lifecycle and string communication for the checked-in binaries. |
| Existing Go fixtures | **TinyGo**, `tinygo build -target wasm-unknown -opt=2 -no-debug -panic=trap` | TinyGo library exports and `_initialize` for the checked-in binaries. |
| Gateway reference and failure fixtures | WABT `wat2wasm` 1.0.42, wasm32 | Exact ABI types, no optional hooks, short-buffer handling, HTTP/configuration/chain integration, concurrency, traps and bad memory. |

The older Rust/TinyGo binaries do not record exact compiler versions. Their
source build commands are documented below, but byte-for-byte reproduction of
those legacy binaries is not claimed. The Go `go.mod` language version alone is
not evidence of support for arbitrary Go runtimes. In particular, standard Go
`GOOS=wasip1 GOARCH=wasm`, JavaScript `GOOS=js`, Go schedulers and all WASI features
are not covered by these tests. The helper under
`shenyu-plugin-wasm-base/src/test/pkg/wasmabi` is for internal tests, not a
published SDK; it clamps large `get_args` counts and discards `put_result` codes,
so it must not be assumed to implement the reference profile's error handling.

## Reproduce the fixtures and tests

The [WASM base README](../shenyu-plugin-wasm-base/README.md) lists the gateway
command and fixture builds. Tests load checked-in `.wasm` files and need Java 17
only; Rust, TinyGo and WABT are only needed to rebuild fixtures. No Docker,
ShenYu Admin, database or external upstream is required. Configuration is sent
directly to the gateway-side subscriber, so admin persistence and network data
sync transports are outside the test's scope.
