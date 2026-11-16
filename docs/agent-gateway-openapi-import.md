<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements. See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Agent Gateway HTTP tool import from OpenAPI

Admin can generate Agent Gateway `mcp.httpTools` plans from a supplied OpenAPI
document and explicitly selected existing gateway routes. Preview is read-only;
publication requires a separate explicit selection. The importer writes the same
rule handle that operators configure manually and reuses the existing Admin
validation, rule publication and gateway synchronization path. It is not a
second MCP-server importer and does not add an HTTP execution engine.

The runtime contract for these plans is described in
[Configured HTTP tools](../shenyu-plugin/shenyu-plugin-agent-gateway/README.md#configured-http-tools).
The importer must not weaken it: generated plans pass both the Admin rule
validator and the gateway `AgentGatewayHttpToolConfig` parser before they can be
published.

## Scope and non-goals

The feature covers:

- supplied OpenAPI 3.0.x JSON, parsed with the existing OpenAPI parser;
- explicit operation selection and explicit gateway method/path mapping;
- generation of tool names, descriptions, the supported `inputSchema` subset,
  `requestTemplate` and `argsPosition`;
- reconciliation with the target rule's `allowedTools`;
- read-only preview, ownership-aware diff, and explicit publication through the
  existing rule API.

It does not:

- fetch OpenAPI documents from remote URLs;
- convert Swagger 2 or interpret OpenAPI 3.1;
- infer gateway routes from OpenAPI paths or `servers`;
- expose a whole document automatically;
- create selectors, rules or upstream routes;
- replace or extend the Agent Gateway HTTP execution path.

## Supported OpenAPI subset

The generator is deliberately fail-closed. An operation that cannot be
represented exactly is reported as `UNSUPPORTED` and cannot be selected for
publication.

| Area | Accepted | Rejected |
| --- | --- | --- |
| Document | OpenAPI `3.0.x` JSON, at most 1 MiB UTF-8, unique object keys | 3.1, Swagger 2, duplicate JSON keys, remote or local `$ref` resolution |
| Operation | `GET` and `POST`, literal relative source paths | Path templates, callbacks, non-empty security requirements |
| Parameters | Query parameters with `style=form`, no `allowReserved`; flat JSON body for POST | Path/header/cookie parameters, `$ref` parameters, duplicate parameters within the same parameter list |
| Schema | Flat scalar `string`, `integer`, `number`, `boolean`; required and description preserved | Arrays, nested objects, `enum`, `format`, `pattern`, numeric bounds, `nullable`, `default`, `oneOf`, additional properties |
| Body | Single `application/json`; body required when it has required properties | Multiple media types, optional bodies with required properties, open-ended schemas |
| Names | Tool names `[A-Za-z0-9_][A-Za-z0-9_.-]{0,63}`; argument names `[A-Za-z_][A-Za-z0-9_]{0,63}` | Invalid names, duplicate selected tool names, unresolved duplicate `operationId` values |
| Bounds | At most 16 selected operations and 16 scalar arguments per tool | More than 16 arguments; runtime tool/path/description limits are enforced by the gateway parser |

`operationId` is a name suggestion, not the import identity. The identity of an
operation is its HTTP method plus original source path, for example
`GET /backend/orders`. Changing the method or source path is a removal plus an
addition, never an inferred rename. An explicit `toolName` may resolve duplicate
`operationId` values.

The source document is not persisted. Only generated plans and ownership
metadata are stored.

## Selection and route mapping

Each mapping selects one operation and one existing gateway route:

```json
{
  "method": "GET",
  "path": "/backend/orders",
  "gatewayPath": "/orders/status",
  "gatewayRuleId": "route-rule-id",
  "toolName": "lookupOrder"
}
```

`toolName` is optional. Source paths and `servers` are never treated as gateway
routes; `gatewayPath` and `gatewayRuleId` must be supplied explicitly.

A mapped route is accepted only when:

- it belongs to `divide` or `springCloud`;
- it and its selector exist in the exact namespace and are enabled;
- the rule uses AND matching and contains an exact `uri = gatewayPath` condition;
- every rule condition is a supported `uri` (`=` or `startsWith`) or
  `req_method` (`=`) condition consistent with the mapping;
- the selector is full-flow, or its conditions are supported and match the
  mapping.

Regex, host or header-based matching and other ambiguous routes are reported as
`CONFLICT`. The check is a conservative Admin snapshot: it does not prove that
the upstream is alive, and it cannot exclude a higher-priority route that also
matches.

The target rule must exist in the exact namespace, belong to `agentGateway`, and
already use `trafficType=mcp`. The target rule's own enabled flag is not
required; importing into a disabled target is a preparation step, and enabling
it remains an operator action.

## Preview contract

`POST /agent-gateway/openapi/preview` accepts the namespace, target rule, a
stable operator-chosen `sourceId`, the document and up to 16 mappings. It
returns a revision, per-operation items and warnings. It performs no writes.

```json
{
  "revision": "sha256-hex",
  "items": [
    {
      "identity": "GET /backend/orders",
      "toolName": "lookupOrder",
      "action": "ADD",
      "before": null,
      "after": "{\"description\":\"...\",\"inputSchema\":{...},\"requestConfig\":{...}}",
      "diagnostics": []
    }
  ],
  "warnings": ["EXTERNAL_CATALOG_UNVERIFIED: ..."]
}
```

Actions are:

- `ADD`: the identity is not owned and the name is free;
- `UPDATE`: the identity is owned, the name is unchanged, and the generated plan
  or gateway mapping differs from the last published plan;
- `UNCHANGED`: the generated plan equals the last published plan;
- `REMOVE`: the identity no longer exists in the complete supplied document;
- `CONFLICT`: the name belongs to a manual tool or another source, a previously
  imported tool was manually edited or removed, or the mapped route fails
  validation;
- `UNSUPPORTED`: the operation cannot be represented by the supported subset.

Deselecting an operation does not propose its removal. Only disappearance from
the complete supplied document produces `REMOVE`, and publication of a removal
requires explicit confirmation.

`before` and `after` are JSON strings so that the preview is plain JSON. `after`
contains only the generated tool plan; it never contains client-supplied raw
tool JSON.

## Publication and ownership

`POST /agent-gateway/openapi/apply` accepts the exact preview request, the
revision, the selected identities, `confirmRemovals` and
`acknowledgeCatalogLimit`.

```json
{
  "request": { "namespaceId": "...", "ruleId": "...", "sourceId": "orders-v1",
               "document": "...", "mappings": ["..."] },
  "revision": "sha256-hex",
  "selected": ["GET /backend/orders", "POST /backend/orders"],
  "confirmRemovals": false,
  "acknowledgeCatalogLimit": true
}
```

Apply locks the target rule row, re-reads current state, regenerates the
preview, rejects a changed revision, rejects selections that contain unknown,
unsupported or conflicting items, and publishes only the selected items in one
transaction. Existing rule conditions and settings are preserved.

Ownership is keyed by namespace, target rule, `sourceId` and source identity.
Multiple sources may coexist in the same target rule. A manual tool is never
adopted or overwritten. Changing an imported tool name is a conflict; the
operator must remove it and import the new identity explicitly.

For each addition or update, the importer:

- writes the plan into `mcp.httpTools`;
- adds the tool name to `mcp.allowedTools` if the import created that grant;
- records the tool name, last published plan, grant ownership and mapped
  `gatewayRuleId` in the source manifest.

For a removal, it removes only the plan, the grant created by that import and
the same-name `structuredContentRemovals` entry. Unrelated grants, filters and
manual configuration are preserved.

`UNCHANGED` selections do not rewrite the rule handle. A source removal requires
`confirmRemovals=true`. Apply also enforces the runtime 16-tool budget against
the merged result and rejects a selection that would exceed it, reporting the
current, selected and final counts before any write. An operation that becomes
unsupported remains published and is not proposed for removal automatically; the
operator must either remove it from the complete source document or manage it
manually.

An operation that was already imported and then manually edited becomes a
`CONFLICT` and blocks update and removal until it is restored or managed
manually.

## Concurrency

The revision is an optimistic concurrency token computed from the request, the
target rule and selector snapshots, the ownership manifest, the mapped route
snapshots, the computed items and the configured reserved names. Object keys are
sorted; array order is preserved. It is not a public tool-contract digest and
makes no backend compatibility promise.

Apply holds `SELECT ... FOR UPDATE` on the target rule, validates against the
locked row, and rejects a preview whose revision no longer matches. Provenance
and rule publication share one transaction; a publication failure rolls back the
manifest update as well. A route or provider can still change after validation;
the gateway runtime remains authoritative.

## Security and errors

Both endpoints require `system:plugin:edit` and namespace membership. The
namespace check returns the established `TOKEN_NO_PERMISSION` result (code 601)
directly; the controller aspect would otherwise wrap a thrown Shiro exception
into a generic admin error.

`shenyu.agent-gateway.openapi.reserved-tool-names` is an optional
comma-separated operator deny list. Preview checks configured `httpTools`,
existing `allowedTools` bindings, other sources' ownership and that list.

Admin cannot observe startup-local Java providers or live remote MCP catalogs.
Every preview therefore reports `EXTERNAL_CATALOG_UNVERIFIED`, and apply requires
`acknowledgeCatalogLimit=true`. This is not a complete collision check: the
gateway's existing runtime collision guard remains authoritative. Related work
on managed publication and policy (#8047), remote catalog freshness (#7952) and
contract snapshots (#7935) does not currently define an Admin-visible catalog of
startup-local providers for import-time collision checks; this remains an
explicit limitation pending maintainer guidance.

## Persistence and migration

Provenance is stored in the Admin-only `agent_gateway_import` table, one row per
target rule:

- `id` primary key, `rule_id` unique index, `namespace_id`;
- `manifest` stores every source ownership map for the rule;
- `date_created` and `date_updated`.

The manifest is not synchronized to gateways and is not read on the request
path. There is no per-apply history; a repeated import updates the row in place.
There is no foreign key to `rule`. Deleting a target rule leaves an unreachable
provenance row; rule IDs are generated UUIDs and are not reused, so it cannot
silently bind to another rule. Operators may remove orphan rows manually.
Configuration export/import does not carry provenance.

Generated plans can exceed the previous H2 `VARCHAR(1024)`, MySQL `TEXT`
(65,535-byte) and operation-log limits, so the 2.7.2-to-2.7.3 migration:

- widens `rule.handle` to `CLOB` on H2 and `MEDIUMTEXT` on MySQL/OceanBase;
- widens `operation_record_log.context` to `MEDIUMTEXT` on MySQL/OceanBase,
  because a rule update snapshots the complete handle into the operation log;
- leaves PostgreSQL/openGauss `TEXT` and Oracle `CLOB` unchanged.

The rule and operation-log mappers bind the large columns as CLOB on Oracle via
`OracleClobStringTypeHandler`; Oracle's JDBC `setString` limit would otherwise
truncate or reject large plans. Fresh H2 schemas create `rule.handle` as CLOB.
The migration guide documents the required backup and restart procedure.

## Verification and boundaries

Tests cover generation against both production parsers, unsupported OpenAPI
features, duplicate names, preview/apply selection, stale revisions, manual-edit
conflicts, source removal, multi-source ownership, namespace isolation and the
provenance mapper. A routing test feeds generated plans through the ordinary
`ShenyuWebHandler` → `AgentHttpToolExecutor` → route chain and verifies the final
upstream URI, UTF-8 and reserved-character query values, POST body mapping,
private-header isolation and JSON result handling.

The MySQL path was exercised against a live MySQL 8.x instance, including a
16-tool import whose handle and manifest exceed 65,535 characters. That run also
confirmed the need to widen `operation_record_log.context`. OceanBase,
PostgreSQL, openGauss and Oracle remain covered by the native SQL matrix.

Known boundaries:

- no dashboard UI in this repository;
- no import history or audit table beyond the existing operation log;
- no automatic cleanup of provenance when a target rule is deleted;
- no collision visibility for startup-local providers or remote catalogs, as
  described above.

## References

- Feature issue: apache/shenyu#7923
- [Configured HTTP tools](../shenyu-plugin/shenyu-plugin-agent-gateway/README.md#configured-http-tools)
- [Agent Gateway API key identities](agent-gateway-key-auth.md)
- [Agent Gateway trusted identity and tools authorization](agent-gateway-principal-authorization.md)
- Agent Gateway HTTP execution: apache/shenyu#7572
- Query duplicate fix used by the routing test: apache/shenyu#7909 and #7926
- Related managed publication and policy work: apache/shenyu#8047
- Remote catalog freshness: apache/shenyu#7952
- Contract snapshots and pinning: apache/shenyu#7935
