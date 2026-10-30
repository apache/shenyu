<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements.  See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License.  You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Standalone file configuration

This optional starter makes a local declarative file the only configuration source for an HTTP gateway. It uses the normal plugin chain, configuration subscribers and plugin/upstream handlers. It does not run Admin, access an Admin database, or configure the gateway through HTTP management endpoints. It is disabled unless `shenyu.sync.file.enabled=true`.

## Run

The Bootstrap dependency includes this starter. For a custom Bootstrap, add `org.apache.shenyu:shenyu-spring-boot-starter-sync-data-file:${shenyu.version}`; it includes the gateway starter and WebFlux runtime. Use the same ShenYu version for all modules.

Copy [example/gateway.yaml](example/gateway.yaml) to a readable external file. Run an HTTP backend on port 8080 with an `/api/hello` endpoint (or set `UPSTREAM_ORIGIN` to your backend's HTTP origin). From an extracted Bootstrap distribution built from this version:

```sh
UPSTREAM_ORIGIN=http://127.0.0.1:8080 \
java -cp 'conf:lib/*:ext-lib/*' org.apache.shenyu.bootstrap.ShenyuBootstrapApplication \
  --shenyu.sync.file.enabled=true \
  --shenyu.sync.file.path=/etc/shenyu/gateway.yaml

curl http://localhost:9195/api/hello
curl http://localhost:9195/unmatched
```

The first request forwards the original path/query to the backend. An unmatched request returns ShenYu's existing `SELECTOR_NOT_FOUND` error (`code: -107`) without forwarding to any backend. HTTP error status formatting follows the existing gateway result configuration.

For a Bootstrap container image built from this version, mount the file read-only and use Spring environment variables:

```sh
docker run --rm -p 9195:9195 \
  -v "$PWD/gateway.yaml:/etc/shenyu/gateway.yaml:ro" \
  -e SHENYU_SYNC_FILE_ENABLED=true \
  -e SHENYU_SYNC_FILE_PATH=/etc/shenyu/gateway.yaml \
  -e UPSTREAM_ORIGIN=http://your-backend:8080 \
  your-shenyu-bootstrap-image
```

The file path is a filesystem path, not a URL or classpath resource. No Admin process, database or configuration center is required. Application settings such as server ports, TLS, HTTP client pools, filters, namespace and logging still belong in Spring application configuration, separately from this file.

## Source ownership and startup

A Boot environment post-processor establishes ownership before auto-configuration constructs clients:

* The shipped `ws://localhost:9095/websocket` default is explicitly suppressed. Other configured WebSocket/HTTP/ZooKeeper/Nacos/etcd/Apollo sync endpoints fail startup; remove them when selecting file mode. Missing, blank and `false` endpoint properties mean disabled. The reserved shipped WebSocket URL is suppressed even when explicitly supplied.
* Admin heartbeat, WebSocket initial-sync readiness and external-JAR plugin loading are disabled in file mode, including inherited Bootstrap defaults. Their normal settings are retained when file mode is disabled.
* Local management controllers and their dispatch filter share one explicit gate: `shenyu.local.enabled=true`. Omitting the property disables both (previously the filter was implicitly enabled and failed closed without a key). Explicit `shenyu.local.enabled=true`, `shenyu.register.enabled=true` and `shenyu.alert.enabled=true` fail startup as conflicting writers/Admin clients. No HTTP management key is needed.
* All configuration is parsed and validated before any entry is applied. Plugins, selectors, rules and upstreams are then applied in dependency order. Undeclared classpath plugins are disabled without initializing their configuration handlers.
* Application runs after singleton initialization and before the web server lifecycle starts. Registered asynchronous handler work must finish within `shenyu.sync.file.apply-timeout` (default `30s`, maximum `5m`). Missing/invalid files, handler failures and timeouts abort startup. A file-mode WebFilter additionally refuses traffic with 503 until application succeeds. Spring publishes normal accepting-traffic readiness only after successful startup; no Admin readiness check is installed.
* Shutdown and startup failure remove file-owned entries through the subscribers. Restarting the same file preserves explicit IDs and replaces cache entries rather than adding generated identities.

The file is read once, with a 1 MiB limit. There is no watching, hot reload, write-back or merging. Edit the file and restart to change configuration. To return to Admin mode, set `shenyu.sync.file.enabled=false` (or remove it), restore the desired sync/heartbeat/runtime settings, and restart. The file-mode property overrides exist only in that process; application files are never rewritten.

## Version 1 schema

YAML and JSON share the same strict schema. Unknown fields, duplicate mapping keys, multiple YAML documents, wrong types, duplicate identities/references, unavailable plugins and ambiguous sort orders fail validation. Diagnostics report a schema path or parser line/column without echoing values or parser source excerpts.

| Entry | Fields and behavior |
| --- | --- |
| Root | Required integer `version: 1`, arrays `plugins`, `selectors`, `rules`, `upstreams`. Optional `metadata` and `auth` must be empty arrays. |
| Plugin | Required `name`; optional boolean `enabled` (default true), object `config` (empty), integer `sort` (must equal the implementation's built-in order). Names must be unique and implementations must be on the classpath. |
| Selector | Required `id`, `plugin`, nonempty `conditions`; optional `name` (defaults to ID), `enabled` (true), `sort` (0), `matchMode` (`and`). Supported plugins: `divide`, `contextPath`, declared under `plugins`. Rules are always matched (`continued=true`); no hidden default rule is synthesized. |
| Rule | Required `id`, `selector` reference, nonempty `conditions`, object `handle`; optional `name`, `enabled`, `sort`, `matchMode` with the selector defaults. The plugin is derived from the referenced selector. |
| Condition | Required string `paramType` (`uri`, `req_method`, `header`, `query`), `operator` (`=`, `match`, `startsWith`, `endsWith`, `contains`, `regex`), `paramValue`. `paramName` is required for headers/query parameters; optional otherwise. Regular expressions are compiled during validation. `match` uses the existing Ant-style matcher. |
| Upstream group | Required `selector` reference to a divide selector and nonempty `targets`. One group per selector. Every divide selector must have upstreams, and every selector must have a rule. |
| Target | Required `url`, an HTTP(S) origin such as `http://backend:8080`, with no user info, path (except `/`), query or fragment. Optional integer `weight` from 1 to 10000 (default 100). Duplicate normalized origins within a selector are rejected. Static targets start enabled, with warmup 0 and active health checks disabled. Existing request-time upstream error handling still applies. |

Plugin names `global`, `divide`, `uri`, `nettyHttpClient`, `response` must be explicitly present and enabled. Their fixed order is -1, 200, 205, 210, 420. Optional `contextPath` runs at order 80 and can be explicitly disabled. Other plugins and alternative HTTP client implementations are outside version 1 and are rejected if declared. These plugins have no supported global `config` fields; keep `config: {}` or omit it. Configure the Netty HTTP client through existing `shenyu.httpclient.*` runtime properties.

Selector IDs are unique across selectors, rule IDs across rules. Use 1–128 letters/digits/underscores/dots/hyphens, starting with a letter/digit. Plugin IDs are deterministically `file-plugin-<name>`; upstream IDs derive from selector ID and target position. Sort values must be nonnegative and unique among selectors of one plugin or rules of one selector. YAML list order does not override explicit sort. `matchMode` is `and` or `or`; empty conditions are rejected—use URI `match` with `/**` for an explicit catch-all.

Supported rule handles:

* `divide`: `loadBalance` (`random` default, or `roundRobin`), integer `retry` (0–10, default 3), `timeout` (1–2147483647 ms, existing default 3000), `headerMaxSize` and `requestMaxSize` (0–2147483647 bytes, default 0/unlimited). Other fields are rejected.
* `contextPath`: string `contextPath` and/or `addPrefix`, each starting with `/`; at least one is required. Existing context-path plugin semantics apply. Configure its own selectors and rules to control where rewriting takes place.

Metadata and application-auth data are not supported in version 1. Omit `metadata`/`auth` or use `[]`; nonempty entries fail startup. RPC routes, dynamic discovery, remote plugin loading and authentication plugins are not supported by this initial HTTP schema. They require a later schema/handler extension rather than silently ignored configuration.

## Environment values

String fields support standard Spring `${NAME}` and `${NAME:default}` placeholders, resolved from the Boot environment **after** YAML/JSON parsing. A placeholder cannot inject YAML structure. Unresolved placeholders fail validation. For example, the sample upstream uses `${UPSTREAM_ORIGIN:http://127.0.0.1:8080}`. Numeric and boolean fields use native YAML/JSON types. Keep credentials in environment/secret mechanisms supported by your deployment; version 1 rejects credentials embedded in upstream URLs. The loader does not log the document, resolved values or exception payloads.

## Verification

`FileModeIntegrationTest` starts real reactive Boot servers with the repository's stock Bootstrap `application.yml`, forwards HTTP to a local backend, rejects unmatched requests and local writes, checks absent sync/heartbeat beans, and restarts with identical IDs. Parser/loader tests cover validation, missing files/plugins/handlers, secret-safe errors and synchronous/asynchronous application failure. File mode makes no throughput or latency claim.
