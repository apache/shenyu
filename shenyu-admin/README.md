# ShenYu Admin API Document

You can check http://localhost:9095/swagger-ui.html to check all restful api and models.

## Client discovery registration

URI registration creates a selector-level local discovery as a fallback. If a
client subsequently registers Zookeeper, Nacos or another supported remote
discovery, Admin replaces only this automatically created fallback and clears its
local upstream observations. A committed takeover also clears the previous runtime
upstreams when the remote registry is empty. The existing handler/relation identity is preserved;
the configured discovery supplies the new upstreams after the transaction commits.

Once a remote discovery owns a selector, later URI registrations do not inject
local upstreams or overwrite the selector handle. Existing manually configured
discoveries are preserved. Registration retries use the same transaction boundary,
and the URI registration's discovery event is published only after commit.

The selector lock coordinates concurrent Admin writers without blocking unrelated
selectors during URI health checks. This prevents new duplicate initial bindings;
pre-existing duplicate relations are not automatically merged.

## Compact Discovery messages (WebSocket only)

`shenyu.sync.websocket.compact-discovery-payload` defaults to `false`. Enabling it projects
Discovery messages at the Admin-to-gateway serialization boundary. It omits the outer
`discoveryHandlerId` and each upstream's `id`, `dateUpdated`, `discoveryHandlerId`, and
repeated `namespaceId`. The enclosing namespace and selector identities, creation timestamp
(for warmup), protocol, URL, manual status, weight, and opaque `props` are preserved.
Empty upstream lists retain their clearing semantics. This is a full runtime item, not a patch.

Both Admin and each gateway must opt in. On an audited gateway, set
`shenyu.sync.websocket.compact-discovery-payload: true` to advertise
`X-Shenyu-Discovery-Payload: compact-v1` on every new connection, including reconnects
and master redirects. Audit that gateway's external plugins and WASM guests first:
they may use fields that built-in handlers do not read.

Admin sends compact messages only when its own option is enabled and the authenticated
connection advertises exactly `compact-v1`. Missing or unknown capabilities receive the
legacy full payload, so both representations can coexist in one namespace. Both options
default to `false`. A new gateway also accepts full messages from an older Admin.
Initial synchronization uses the requesting connection's capability. Each required variant
is serialized at most once per namespace/event, and unused variants are not built.
Disable either side and reconnect/resynchronize to restore full messages. Configure all
Admin nodes consistently; older Admin versions do not negotiate capabilities.

This option does not change internal Admin events, cluster forwarding, HTTP polling,
registry-backed synchronization, selectors/rules, or the WebSocket envelope and initial-sync
completion protocol. No extra JSON parse/delete/serialize pass is used to build compact messages.
Periodic reconciliation still sends full snapshots, which compact-capable gateways must accept.
