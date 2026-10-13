# WASM request bridge

The no-argument constructor of `AbstractShenyuWasmPlugin` registers a request
bridge for the `shenyu.get_args` and `shenyu.put_result` imports. A subclass only
needs its plugin name, order and a WASM resource named after its fully qualified
Java class:

```java
package com.example.gateway;

public final class PolicyPlugin extends AbstractShenyuWasmPlugin {
    @Override
    public String named() {
        return "policy";
    }

    @Override
    public int getOrder() {
        return 100;
    }
}
```

Put the guest module at `src/main/resources/com.example.gateway.PolicyPlugin.wasm`.
It exports `doExecute(i64) -> ()`; `before(i64) -> ()` and `after(i64) -> ()` are
optional. The host passes an invocation ID, not a pointer to a Java object.

## Input and configuration

`get_args` supplies one UTF-8 JSON snapshot per invocation:

```json
{
  "version": 1,
  "phase": "doExecute",
  "request": {
    "method": "GET",
    "uri": "https://example.org/orders?tag=one&tag=two",
    "path": "/orders",
    "headers": {"X-Client": ["first", "second"]},
    "queryParams": {"tag": ["one", "two"]}
  },
  "plugin": {"name": "policy", "enabled": true, "config": "{\"mode\":\"strict\"}"},
  "selector": {"id": "selector-1", "pluginName": "policy", "handle": "{}"},
  "rule": {"id": "rule-1", "selectorId": "selector-1", "handle": "{\"deny\":true}"}
}
```

The configuration objects are serialized from real `PluginData`, `SelectorData`
and `RuleData` DTOs, including their other fields and condition lists. The
example shows only some fields. `config` and `handle` retain their DTO string
representation: guests parse those strings separately if they contain JSON.
Guests should accept additional input fields and treat header names without
regard to case. URI and path retain their encoded representation.

The plugin configuration comes from `BaseDataCache`. Selector and rule are the
objects selected by `AbstractShenyuPlugin`, including its synthetic default rule
when a selector does not continue matching. For `before` and `after`, the phase
changes and selector/rule are JSON null. Only `doExecute` results affect HTTP
processing; hook results are validated and discarded.

Request bodies, response bodies and arbitrary exchange attributes are not
serialized. Reading the bridge does not subscribe to or consume the request
body. Request method, URI and body changes are outside this protocol.

### Updates across independent instances

A WASM data handler and request plugin have separate loaders and guest memories.
Use the normal `CommonPluginDataSubscriber` configuration flow:

1. A sync channel calls `onSubscribe`, `onSelectorSubscribe` or `onRuleSubscribe`
   with the DTO received from admin.
2. The subscriber updates the gateway's Java cache and invokes the registered
   `PluginDataHandler`. It also invalidates affected matching caches.
3. A subsequent matched request serializes the selected DTOs and current plugin
   configuration into its own guest memory. The guest evaluates that input.

This pattern needs no copy from the handler's guest memory. The handler and
plugin must use the same plugin name. Direct calls to a handler alone do not
update the gateway caches; use the subscriber in manual integrations as well.
Plugin callbacks run before the plugin cache write; selector/rule callbacks run
after their cache writes. Visibility is guaranteed after the subscription call
returns, rather than as one atomic transaction across several configuration
events. Already serialized invocations retain their snapshot.

Deletion uses `unSubscribe`, `unSelectorSubscribe` and `unRuleSubscribe`.
Deleted or disabled data then follows normal base-plugin matching behavior and
may skip the guest entirely. Do not keep a second, authoritative copy of
selector/rule configuration in the request guest. If an extension maintains
derived Java state, share that state explicitly between its custom callbacks
and handler; instance-local guest globals are not shared.

`WasmBridgeTest` demonstrates this flow with a separately instantiated
`AbstractWasmPluginDataHandler`, real subscription updates/deletions, and a guest
that changes from continuation to HTTP 403 when the matched rule changes.

## Result protocol

Call `put_result` at most once with a UTF-8 JSON object. Version and action are
required. Returning without submitting a result continues the chain.

| Action | Behavior | Optional fields |
| --- | --- | --- |
| `CONTINUE` | Pass the exchange to the next Java hook/chain | `requestHeaders`, `responseHeaders` |
| `REJECT` | Finish the response without continuing the chain | `body`, `responseHeaders`; `status` is required and must be 400–599 |

```json
{"version":1,"action":"CONTINUE","requestHeaders":{"X-Policy":["checked"],"X-Remove":[]}}
```

```json
{"version":1,"action":"REJECT","status":403,"body":"denied","responseHeaders":{"Content-Type":["text/plain;charset=UTF-8"]}}
```

Header values are arrays of strings. They replace existing values; an empty
array removes the header. Request header changes are passed in a decorated
exchange and do not mutate the original request. Response header changes are
applied before continuation or rejection. A rejection body defaults to empty;
its default content type is `text/plain;charset=UTF-8`, and the host sets its
content length. Normal downstream response handling still governs responses
after `CONTINUE`. These response header changes are not rolled back if the
downstream chain fails.

Framing and hop-by-hop headers cannot be changed: `Host`, `Content-Length`,
`Transfer-Encoding`, `Connection`, `Keep-Alive`, `Proxy-Authenticate`,
`Proxy-Authorization`, `TE`, `Trailer` and `Upgrade`. Invalid header names,
control characters, duplicate header names differing only by case, unknown
result fields and unsupported field/action combinations are errors. Results
must be strict JSON: duplicate JSON keys, trailing documents, malformed UTF-8
and invalid types are rejected.

## Buffers, errors and lifecycle

The imports retain the existing signatures:

```text
shenyu.get_args   (argument_id: i64, address: i64, capacity: i32) -> i32
shenyu.put_result (argument_id: i64, address: i64, length: i32)   -> i32
```

Addresses refer to the guest's linear memory. The address is range-checked
before conversion to Chicory's integer offset; negative/overflowing addresses,
negative lengths and ranges outside memory are errors.

- Input and result payloads are each limited to 1 MiB.
- `get_args(id, 0, 0)` returns the negative required byte length. Allocate that
  many bytes and retry. An undersized buffer returns the same negative length
  and writes nothing. A sufficient buffer receives the entire JSON document and
  returns its positive length; the document has no NUL terminator.
- `put_result` returns zero on success. The host copies and validates the result
  immediately, so later guest-memory writes cannot alter it.
- Invalid host calls throw a `ShenyuWasmException` and abort the invocation.
  Serialization failures, invalid results and guest runtime exceptions during
  `doExecute` yield HTTP 500 without calling the chain or applying guest changes.
  Hook errors propagate to the hook caller.
- Invocation state is released in `finally` after guest execution, including
  traps. IDs expire at that point and cannot be reused to read another request.
  No exchange references or result maps survive for a downstream subscription.
- Request guest calls run on subscription. Calls to one default-bridge plugin
  instance, including hooks, are serialized because its linear memory is shared.
  The lock is released before the reactive chain runs. Downstream cancellation
  cannot retain bridge state. This does not add a guest execution timeout.
- Close the plugin at shutdown (for example, with a Spring bean destroy method).
  `close()` is idempotent; the default bridge refuses further invocations.
  The guest instance still owns its linear memory until it is garbage collected.

### Complete guest example

This Rust example uses `serde_json` to evaluate the current rule's `handle`
string on every request. Build a `cdylib` for `wasm32-unknown-unknown` with
`serde_json = "1"`, and name the resulting resource for the Java subclass above.

```rust
use serde_json::{json, Value};

#[link(wasm_import_module = "shenyu")]
extern "C" {
    fn get_args(id: i64, address: i64, capacity: i32) -> i32;
    fn put_result(id: i64, address: i64, length: i32) -> i32;
}

#[no_mangle]
pub unsafe extern "C" fn doExecute(id: i64) {
    let required = get_args(id, 0, 0);
    assert!(required < 0 && required >= -(1024 * 1024));
    let mut buffer = vec![0_u8; (-required) as usize];
    let written = get_args(id, buffer.as_mut_ptr() as i64, buffer.len() as i32);
    assert_eq!(written as usize, buffer.len());

    let input: Value = serde_json::from_slice(&buffer).unwrap();
    assert_eq!(input["version"], 1);
    let handle = input["rule"]["handle"].as_str().unwrap_or("{}");
    let rule: Value = serde_json::from_str(handle).unwrap();
    let result = if rule["deny"].as_bool().unwrap_or(false) {
        json!({"version": 1, "action": "REJECT", "status": 403, "body": "denied"})
    } else {
        json!({
            "version": 1,
            "action": "CONTINUE",
            "requestHeaders": {"X-Policy": ["checked"]}
        })
    };
    let output = serde_json::to_vec(&result).unwrap();
    assert_eq!(put_result(id, output.as_ptr() as i64, output.len() as i32), 0);
}
```

A normal admin rule update from `{"deny":false}` to `{"deny":true}` becomes
visible through the subscription/cache flow above; the request plugin need not
be re-created and the handler need not share its guest memory.

## Custom host functions and compatibility

### Migrating subclasses that used the no-argument constructor

Previously the no-argument constructor registered no-op host callbacks. It now
selects the JSON bridge, including strict result parsing, a 1 MiB payload limit
and HTTP 500 on invalid results. Existing subclasses using that constructor
must supply a compatible guest or explicitly select their previous adapter.
Use `super((Consumer<Store>) null)` (with `java.util.function.Consumer` imported)
to retain the built-in no-op callbacks, or pass the existing custom initializer
to retain application-specific host functions. The five-argument `doExecute`
hook is now concrete and continues the chain by default; `getArgumentId` is
concrete and allocates unique IDs. Existing overrides remain compatible.

The constructors therefore select **JSON bridge** versus **custom adapter**.
Shutdown cleanup requires bean destruction wiring, for example
`@Bean(destroyMethod = "close")`; this PR adds no starter registration for
external subclasses. Calling `close()` directly is also supported.


The `Consumer<Store>` constructor retains the custom-adapter behavior. It skips
the supported bridge: the consumer registers all required application imports,
and the subclass can override the five-argument `doExecute` hook and
`getArgumentId` as before. `ARGUMENTS` is available during that legacy guest
call and is removed in `finally`. Custom adapters own their result mapping,
buffer policy, synchronization and cleanup.

Passing null explicitly to that constructor retains `WasmLoader`'s built-in
no-op callbacks. It does not select the supported bridge. Use the no-argument
constructor to select the bridge.

Existing custom-constructor Go/Rust tests remain regression tests for this path.
The default five-argument Java hook now delegates to the chain, and the default
`getArgumentId` generates unique IDs. With the supported bridge, only
`CONTINUE` invokes the five-argument hook; `REJECT` completes the response.

## Verification and fixture regeneration

From the repository root:

```bash
./mvnw -pl shenyu-plugin/shenyu-plugin-wasm-base -am test \
  -Dtest=WasmBridgeTest,WasmRequestBridgeTest,AbstractShenyuWasmPluginTest \
  -Dsurefire.failIfNoSpecifiedTests=false
```

The bridge tests use a small committed WASM fixture built from
`src/test/wasm-bridge/bridge.wat`. Its mode 3 evaluates the serialized rule
handle; other modes exercise result handling and failures. The minimal fixture
also verifies that guests may omit the optional before/after hooks. Rebuild them
with WABT:

```bash
cd shenyu-plugin/shenyu-plugin-wasm-base
wat2wasm src/test/wasm-bridge/bridge.wat \
  -o 'src/test/resources/org.apache.shenyu.plugin.wasm.base.WasmBridgeTest$BridgePlugin.wasm'
wat2wasm src/test/wasm-bridge/bridge.wat \
  -o 'src/test/resources/org.apache.shenyu.plugin.wasm.base.WasmBridgeTest$BridgeHandler.wasm'
wat2wasm src/test/wasm-bridge/minimal.wat \
  -o 'src/test/resources/org.apache.shenyu.plugin.wasm.base.WasmBridgeTest$MinimalPlugin.wasm'
```

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

`.github/workflows/wasm-fixtures.yml` runs WABT 1.0.42 and Rust 1.90.0 whenever
the WASM modules change. `python3 .github/scripts/verify-wasm-fixtures.py` rebuilds
all available WAT families into a temporary directory and compares every byte
with the committed resources. It then compiles the five existing Rust guests
for `wasm32-wasip1` and runs the Java WASM suites against the rebuilt resources,
restoring the original resource bytes even on failure. Legacy Rust compiler
versions were not recorded, so those guests are verified by behavior rather
than by an exact-byte claim. Existing TinyGo guests also run in those suites.
`--wat-only` performs just the exact WAT check; `--rust-only` performs just the
Rust rebuild and Java verification. Maven-only builds still use committed guests.
