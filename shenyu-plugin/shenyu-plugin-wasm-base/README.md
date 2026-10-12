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
