# ShenYu Admin API Document

You can check http://localhost:9095/swagger-ui.html to check all restful api and models.

## Compact Discovery messages (WebSocket only)

`shenyu.sync.websocket.compact-discovery-payload` defaults to `false`. Enabling it projects
Discovery messages at the Admin-to-gateway serialization boundary. It omits the outer
`discoveryHandlerId` and each upstream's `id`, `dateUpdated`, `discoveryHandlerId`, and
repeated `namespaceId`. The enclosing namespace and selector identities, creation timestamp
(for warmup), protocol, URL, manual status, weight, and opaque `props` are preserved.
Empty upstream lists retain their clearing semantics. This is a full runtime item, not a patch.

Audit every gateway consuming that Admin before enabling it, including old versions,
external plugins and WASM guests: they may use fields that built-in handlers do not read.
There is no per-session capability negotiation in this first, opt-in WebSocket implementation.
Leave the option disabled for unaudited or mixed fleets. Disable it and resynchronize/reconnect
consumers to return to the legacy payload. Configure all Admin nodes consistently.

This option does not change internal Admin events, cluster forwarding, HTTP polling,
registry-backed synchronization, selectors/rules, or the WebSocket envelope and initial-sync
completion protocol. No extra JSON parse/delete/serialize pass is used to build compact messages.
