# Registration protocol v1

`org.apache.shenyu:shenyu-register-protocol` is owned and released by `apache/shenyu`.
It contains only JDK-based wire models, endpoint/version/response constants, and the
`registration/v1/*.json` compatibility fixtures packaged in the ordinary JAR.
The existing `org.apache.shenyu.register.common` DTO, event and path packages are
preserved. `shenyu-register-common` depends on this artifact, so existing Maven
consumers continue to receive these classes transitively. Do not combine an old
`shenyu-register-common` JAR containing the DTOs with a newer protocol JAR.

## Version and response contract

- `X-Shenyu-Register-Protocol-Version: 1` selects v1. An absent header also selects v1.
- An explicitly empty, unsupported or repeated version header receives HTTP 400,
  `text/plain;charset=UTF-8`, body `Unsupported registration protocol version`,
  before registration or heartbeat processing.
- `X-Shenyu-Client-Version` is optional diagnostic information. Admin does not use
  the client artifact version as an authorization or compatibility decision.
- Admin responses on the registration endpoints report
  `X-Shenyu-Register-Protocol-Version: 1` and
  `X-Shenyu-Register-Capabilities: metadata,uri,api-doc,discovery-config,mcp,offline,heartbeat`.
  A capability means that Admin implements that registration endpoint. It does not
  promise that the matching gateway plugin is enabled. Older Admin versions may
  omit both response headers; clients must not require these additive headers.
- Successful registration and heartbeat responses remain HTTP 200 with the exact
  unquoted body `success`. Registration acknowledges enqueueing, not completion of
  asynchronous persistence or gateway synchronization. Wait for the resulting
  Admin/gateway state when checking end-to-end behavior.
- Existing authentication is unchanged: clients log in via `POST /platform/login`
  with `userName` and `password`, and send the returned `data.token` as
  `X-Access-Token`. The login response is the existing `{code,message,data}` Admin
  envelope. Protocol negotiation neither authenticates nor bypasses authentication.
- Non-success responses and transport failures must retain the existing client
  retry/lifecycle policy. A rejected explicit version requires configuration or
  client correction; sending it repeatedly cannot negotiate another version.

## Endpoints

Every request below is `POST` with `Content-Type: application/json`. Paths, casing,
field types, authentication behavior, and success bodies are part of v1.

| Path | Wire DTO | Fixture | Effect |
| --- | --- | --- | --- |
| `/shenyu-client/register-metadata` | `MetaDataRegisterDTO` | `metadata.json` | Register selectors/rules and optional metadata |
| `/shenyu-client/register-uri` | `URIRegisterDTO` | `uri.json`, `uri-heartbeat.json` | Register an upstream or renew it according to `eventType` |
| `/shenyu-client/register-apiDoc` | `ApiDocRegisterDTO` | `api-doc.json` | Register/offline API documentation |
| `/shenyu-client/register-discoveryConfig` | `DiscoveryConfigRegisterDTO` | `discovery-config.json` | Configure Admin discovery and listener path |
| `/shenyu-client/register-mcp` | `McpToolsRegisterDTO` | `mcp.json` | Register MCP metadata and tool configuration |
| `/shenyu-client/offline` | `URIRegisterDTO` | `offline.json` | Enqueue the supplied URI event; send `OFFLINE` explicitly |
| `/instance/beat` | `InstanceBeatInfoDTO` | `heartbeat.json` | Renew Admin's instance health record |

## Field contract

“Required” below means needed for a useful registration in the receiving service;
legacy controllers do not synchronously validate every field. Preserve that
behavior in v1: an HTTP acknowledgement is not proof that incomplete registration
succeeded. The field sets below describe all serialized DTO fields. Read-only
`getType()` values are server dispatch information, not required request fields.
No fields are designated deprecated in this initial contract.

### Metadata

- Required: `appName`, `rpcType`, and `path` (strings); supply `contextPath` for a
  context-scoped selector. `ruleName` supplies the desired rule name. RPC-specific
  integrations additionally need `serviceName`, `methodName`, `parameterTypes`,
  and `rpcExt` (all strings). `rpcExt` is JSON encoded **inside a string**.
- Optional strings: `pathDesc`, `host`, `namespaceId`. `port` is an integer;
  `pluginNames` is a string array. Missing reference fields remain null, including
  a missing plugin list; this is not an empty-list default.
- Missing `enabled`, `registerMetaData`, and `addPrefixed` default to `false` during
  HTTP deserialization. Clients should explicitly send intended values. Missing
  `timeMillis` defaults to `0`; Java builder timestamps are not wire defaults.
- A missing/null top-level `namespaceId` becomes
  `649330b6-c2d7-4edc-be8e-8a54df9eb385`. Non-null namespace values are preserved.

### URI and offline lifecycle

- Required: `host` (string), `port` (integer), `rpcType` (string), and a non-empty
  `contextPath` or `appName` (strings) identifying the selector. `protocol` is a
  string such as `http://`; it is distinct from the registration protocol version.
- Optional: `namespaceId` has the metadata default; `instanceInfo` is a nullable
  JSON-encoded string for instance diagnostics. Do not remove it from the wire
  model or change its type to an object.
- Missing/null `eventType` is dispatched as `REGISTER` by the URI subscriber.
  `HEARTBEAT` renews the upstream; `OFFLINE` removes it according to the registration
  service's existing lifecycle. The `/offline` path alone does not change the event.
- Existing enum spellings are `REGISTER`, `UPDATED`, `DELETED`, `IGNORED`, `OFFLINE`,
  `HEARTBEAT`. Preserve all spellings; the current URI subscriber dispatches only
  `REGISTER`, `HEARTBEAT`, and `OFFLINE`. Do not reinterpret the other values as
  removal events during extraction. In particular legacy shutdown's `DELETED`
  payload is not evidence of an `OFFLINE` transition.

### API documentation

- Required for registration: `contextPath`, `apiPath`, `rpcType` (strings),
  `httpMethod` (integer, existing HTTP-method code), and `eventType: REGISTER`.
  `OFFLINE` uses `contextPath` to offline the corresponding documentation.
- Optional strings: `consume`, `produce`, `version`, `ext`, `apiOwner`, `apiDesc`,
  `document`. Optional integers: `state`, `apiSource`. `tags` is a string array;
  a missing/empty list falls back to the context path in the service.
- `ext` is a JSON-encoded string. Its `ApiExt` model has string fields `protocol`,
  `host`, `serviceName`, `methodName`, `parameterTypes`, `rpcExt`, integer `port`,
  and boolean `addPrefixed` (missing defaults to false).
- The existing API-doc wire model has no namespace field. An unknown namespace
  property must not be treated as a new namespace-scoping contract in v1.
- Missing reference fields, including `eventType`, remain null. Unlike URI events,
  the API-doc service requires an explicit event; it has no `REGISTER` default.

### Discovery configuration and instances

- Required strings: `selectorName`, `name`, `discoveryType`, `serverList`,
  `listenerNode`, `pluginName`; `handler` is a JSON-encoded string (usually `{}`).
  `props` is an object of provider configuration properties. Supply `{}` when no
  additional provider settings are needed; missing `props` remains null.
- Optional `namespaceId` has the metadata default. `selectorName` must match the
  selector owned by the chosen plugin and namespace.
- Business instances register through the main-owned `shenyu-registry-api`,
  independently of this HTTP configuration request. `InstanceEntity` contains
  `appName`, `host`, `port`, `uri`, `status`, and `weight`; its provider storage
  format remains owned by the registry API, not a copied client model.
- ZooKeeper registers ephemeral nodes below
  `/shenyu/register/instance/<appName>`; use the path produced by
  `InstancePathConstants.buildInstanceParentPath(appName)` for `listenerNode`.
  Closing the registry session removes the instance and Admin publishes updated
  upstream snapshots. Gateway sync remains an Admin responsibility.

### MCP

- `metaDataRegisterDTO` is required and follows the metadata contract above.
  `mcpConfig` is a required JSON-encoded string consumed as the MCP rule handle.
  Its generated content includes `name`, `description`, `parameters`, and
  `requestConfig` (itself a JSON-encoded string).
- The outer `namespaceId` defaults like metadata. Send the same namespace in
  nested metadata; v1 does not promise recursive namespace defaulting.
- Shared annotation-independent tool models and generators live in
  `shenyu-register-mcp`, outside the dependency-free wire artifact. Client
  annotation adapters remain in `shenyu-client-mcp-common`.

### Instance heartbeat

- Identity: `instanceIp`, `instancePort`, `instanceType`, `namespaceId` are strings.
  **`instancePort` is a string**, unlike the integer URI `port`.
- `instanceInfo` is an optional JSON-encoded string. Missing values remain null;
  `/instance/beat` does not apply the registration controller's namespace default.
- First receipt creates an in-memory ONLINE record; subsequent receipts renew its
  last-beat time. Existing periodic checks mark it OFFLINE after 20 seconds and
  DELETED after 60 seconds without a beat, and synchronize state to the database.
  These are Admin runtime settings/behavior, not client-controlled DTO fields.
- The bootstrap heartbeat sender and its starter remain in main. Its HTTP transport
  lives in `shenyu-register-common`; it must not resolve the business client SDK.

## Compatibility and release window

Admin supports v1, including headerless legacy v1 requests, regardless of the
client artifact release number. This is a **wire-contract support window**, not a
claim that every historic client binary or Admin release has been E2E certified.
Unknown JSON fields, including nested metadata fields, are ignored. Additive fields
must be optional and defaultable without changing existing semantics. Preserve
existing enum values; a new value must not be sent to an older receiver without
capability/version negotiation. Field removals, type/semantic changes, endpoint
changes, or incompatible authentication/success responses require a new protocol
version. Before retiring v1, ship at least one Admin release accepting both v1 and
its successor and publish the retirement boundary; this change sets no v1 EOL.

`RegistrationProtocolCompatibilityTest` replays these fixtures with absent/v1
headers, unknown fields, optional-field defaults, and rejected versions. The real
ZooKeeper integration test replays discovery JSON over authenticated HTTP and
verifies persistence and WebSocket snapshots using main-owned registry APIs.
Keep historic fixtures immutable; add fixtures for new optional combinations.
See [the migration guide](../CLIENT_MIGRATION.md) for release gates and CI scope.
