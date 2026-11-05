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

# Configuration import preview

Admin can produce a bounded, read-only plan for an existing namespace
configuration archive before running the live import. The plan reports which
objects the supported sections would create, reuse, skip or reject, including
reference remapping, missing dependencies and import-level validation failures.

Preview does not write rows, publish change events, synchronize gateways or
reserve identifiers. It is an observation of the target state at a point in
time, not a reserved transaction and not proof of future Gateway activation.
Applying an archive still requires fresh authorization and validation.

The feature is backend-only and does not add a table or a persisted preview
entity. The required `previewToken` described below is a stateless digest that
detects archive or baseline drift at import time.

## Scope and non-goals

The first delivery covers:

- an existing import archive and an authorized target namespace;
- the existing import archive size and entry-count limits;
- `plugin_template.json` (global plugin templates), `namespace_plugin.json`,
  `selector.json` and `rule.json`;
- deterministic create/reuse/skip/reject outcomes with source references,
  target matches, reference remapping and bounded diagnostic reasons;
- a read-only preview API and a required drift token on the normal import.

It does not:

- implement transactional runtime revisions or activation;
- implement fleet rollout, canary or application acknowledgements;
- replace or extend configuration export;
- fix the existing namespace-import transaction behavior;
- provide approval, rollback or credential-rollback workflows;
- add a Dashboard UI;
- validate sections outside the four supported sections.

Related work remains separate: revisions and activation (#8060), fleet rollout
(#8193), export (#8301/#8296), the import transaction bug (#6620),
acknowledgements and rollout diagnostics (#7719/#8066), authorization evolution
(#8177/#8178) and Kubernetes/GitOps ownership (#8070).

## API

```http
POST /configs/import/preview
Content-Type: multipart/form-data
Permission: system:manager:importConfig

namespace: <target namespace>
file:      <import archive>
```

The response is a `ShenyuAdminResult` whose `data` is the preview report.
Errors use the established Admin convention of HTTP 200 with a body `code`:

- `400`: missing parameters, unknown namespace, malformed or oversized archive;
- `429`: concurrent preview limit reached;
- `500`: unexpected failure, with a generic message.

Authentication failures are handled by the existing Shiro filter and return
HTTP 401. Preview uses the same permission and namespace check as the normal
import.

The normal import requires a token from a matching preview:

```http
POST /configs/import
Content-Type: multipart/form-data

namespace:    <target namespace>
file:         <import archive>
previewToken: <token from a matching preview>
```

Admin recomputes the token before invoking any import handler:

- missing token -> body `code=400`
  (`previewToken is required; preview the archive before importing`);
- mismatched token -> body `code=409`
  (`Preview is stale; preview again before importing`).

```json
{"code": 409, "message": "Preview is stale; preview again before importing", "data": null}
```

This is an intentional breaking change for callers that previously imported
without a preview; they must call `/configs/import/preview` first.

## Report contract

```json
{
  "observedAt": "2026-11-04T15:54:25Z",
  "archiveDigest": "sha256:...",
  "baselineFingerprint": "sha256:...",
  "previewToken": "sha256:...",
  "targetNamespace": "ns",
  "fullyValidated": true,
  "truncated": false,
  "summary": {"create": 4, "reuse": 1, "skip": 1, "reject": 0},
  "sections": [
    {
      "section": "selector.json",
      "scope": "NAMESPACE",
      "supported": true,
      "present": true,
      "blocking": false,
      "validatedBy": ["identity-resolution"],
      "outcomes": [
        {
          "sourceRef": "selector.json:fixture-selector#fixture-sel-src",
          "sourceName": "fixture-selector",
          "targetRef": "new",
          "action": "CREATE",
          "remappedRefs": ["plugin:fixture-sign-src -> 1"],
          "diagnostics": []
        }
      ],
      "diagnostics": []
    }
  ],
  "warnings": [
    "OBSERVATION_ONLY: ...",
    "PREVIEW_TOKEN: ..."
  ]
}
```

Actions are limited to four values:

- `CREATE`: the import would insert a new object;
- `REUSE`: no insert; the source maps to an existing target or a target planned
  earlier in the same archive;
- `SKIP`: no insert and no downstream reference remapping, for example a
  duplicate rule name or namespace plugin relation;
- `REJECT`: the object cannot be imported, for example a missing parent or an
  invalid handle.

Preview never emits delete, replace or ownership-change actions.

`targetRef` is one of `existing:<id>`, `existing` when an existing row has no
usable id, `new`, `planned` or `none`. Ephemeral ids generated while planning
are never returned.

`fullyValidated` is a coverage flag, not a success flag. It is false when an
unsupported or unknown section is present, a supported section cannot be
parsed, the report is truncated, or an object cannot be validated because a
required dependency or identity is missing. It can be true when outcomes contain
other `REJECT` results, such as an invalid rule handle that was correctly
validated.

`blocking` means the live importer would throw and roll back that section, for
example a rule handle validation failure or malformed section JSON. Per-object
outcomes remain diagnostic; the live import is fail-fast.

`truncated` means the outcome budget stopped planning. Later sections are marked
`REPORT_TRUNCATED` and are not fully analyzed.

Diagnostics are stable codes rather than arbitrary exception text:
`INVALID_SOURCE`, `INVALID_PLUGIN`, `INVALID_NAMESPACE_PLUGIN`,
`INVALID_SELECTOR`, `INVALID_RULE`, `INVALID_PLUGIN_CONFIG`,
`INVALID_RULE_HANDLE`, `INVALID_CANARY_CONFIG`, `MISSING_PLUGIN_TEMPLATE`,
`MISSING_SELECTOR_REFERENCE`, `DUPLICATE_SELECTOR_NAME`,
`DUPLICATE_NAMESPACE_PLUGIN`, `DUPLICATE_RULE_NAME`, `NAMESPACE_REMAPPED`,
`EMBEDDED_RULES_IGNORED`, `GLOBAL_EFFECT`, `SECTION_NOT_SUPPORTED`,
`UNKNOWN_SECTION`, `SECTION_MALFORMED`, `REPORT_TRUNCATED` and
`COVERAGE_GAPS_PRESENT`.

## Section semantics

| Section | Import behavior | Preview outcome |
| --- | --- | --- |
| `plugin_template.json` | Match global plugins by name. Existing names are reused and remapped; new names are inserted. | `REUSE`; `CREATE` with `GLOBAL_EFFECT` |
| `namespace_plugin.json` | Map source plugin ids to target plugin ids; existing relations are skipped; new relations are inserted. | `SKIP` with `DUPLICATE_NAMESPACE_PLUGIN`; `CREATE`; missing plugin mapping is `REJECT` |
| `selector.json` | Match selectors by mapped plugin id and name; duplicates are skipped but references are remapped; new selectors are inserted. | `REUSE` with `DUPLICATE_SELECTOR_NAME`; `CREATE`; missing plugin mapping is `REJECT` |
| `rule.json` | Require a mapped selector; skip duplicate names; validate canary and plugin handles; insert new rules. | `SKIP` with `DUPLICATE_RULE_NAME`; `CREATE`; missing selector is `REJECT`; validation failure is `REJECT` and marks the section blocking |

All other known sections, `plugin.json` and unknown archive entries are reported
with `supported=false`. Their content is not parsed. `auth.json`,
`plugin_handle.json`, `discovery.json`, `discovery_upstream.json`, `dict.json`,
`meta.json` and `proxy_selector.json` are therefore never inspected or echoed by
preview. They still execute during the real import, which is why their presence
makes `fullyValidated=false`.

A source `namespaceId` that differs from the target namespace adds
`NAMESPACE_REMAPPED` to the outcome. Non-empty `selectorRules` in a selector
document add `EMBEDDED_RULES_IGNORED`, matching the current import behavior.

## Global plugin templates and namespace scope

`plugin_template.json` has a global effect. Its section is reported with
`scope=GLOBAL`; a new global plugin carries `GLOBAL_EFFECT`, and the report
contains a `GLOBAL_PLUGIN_TEMPLATE` warning. A namespace preview is not
permission to create a global template: preview never creates one, and the live
import remains subject to the existing import permission.

Namespace-local sections are reported with `scope=NAMESPACE`. Target data is
only read from the target namespace plus the global plugin list needed for
identity resolution.

## Baseline, digest and drift

`archiveDigest` is the SHA-256 digest of the uploaded archive bytes.

`baselineFingerprint` is a SHA-256 digest over sorted identity rows:

- global plugins: `id, name`;
- namespace plugin relations: `id, pluginId`;
- selectors: `id, pluginId, selectorName`;
- rules: `id, selectorId, ruleName`;
- canary dictionaries used by rule validation: `id, type, dictValue, enabled` for
  the `paramType` and `operator` types.

Configuration values, handles and timestamps are excluded because they do not
change create/reuse/skip decisions and should not enter the hash input. The
fingerprint is conservative: unrelated target rows can invalidate a preview.

`previewToken` binds the version marker, namespace, archive digest and baseline
fingerprint. It is stateless, has no TTL and cannot be revoked. It detects a
different archive or a changed baseline at import-check time; it is not a lock,
and a baseline can still change after the check. Import sections retain their
existing transaction boundaries, so no atomicity claim is made beyond the
implemented check.

## Limits, redaction and concurrency

Preview reuses the existing import limits:

```yaml
shenyu:
  config:
    import:
      max-entry-size: 104857600
      max-total-size: 209715200
      max-entry-count: 1000
      preview:
        max-outcomes: 1000
        max-diagnostics-per-outcome: 5
        max-ref-length: 128
        max-concurrent: 1
```

The strict archive reader also rejects malformed ZIPs, duplicate entry names and
blank entry names before the target snapshot is loaded.

Reports contain only names, ids, reference descriptions and fixed diagnostic
codes. Plugin `config`, selector/rule `handle`, plugin `file`, application auth
secrets and plugin handle values are never returned. Unsupported sections are
not parsed. Report text is bounded, refs and diagnostics are truncated to the
configured length, and the outcome budget stops planning with
`REPORT_TRUNCATED`.

A per-instance semaphore bounds concurrent previews. The default is one preview
per Admin instance; the limit is not cluster-wide. A saturated instance returns
body `code=429`. `matchesToken` performs a short read-only check and is not
covered by the preview semaphore.

Section order follows `ConfigsExportImportEnum` import order. Outcomes follow the
archive source order, and unknown section entries are sorted. Baseline rows are
sorted before hashing, so ZIP entry order does not change the semantic plan.
The token itself changes when the archive bytes change.

## Implementation and compatibility

Preview reads a target identity snapshot through the existing mappers and plans
from the archive DTOs. It shares the same identity resolution and validation
used by the namespace import:

- `PluginTemplateImportResolver`;
- `NamespacePluginImportResolver`;
- `SelectorImportResolver`;
- `RuleImportResolver`;
- `RuleImportValidator`.

The preview never calls `importData`, an event publisher or the synchronization
service. The existing `ConfigsExportImportHandler` interface is unchanged; the
preview service reports unsupported sections directly from
`ConfigsExportImportEnum`.

No database DDL is introduced. For valid archives, existing import behavior,
duplicate/skip semantics, reference mapping, event publication and gateway
synchronization are unchanged. Missing parameters and unknown namespaces now
return body `code=400` from both preview and import. Import now requires a
`previewToken`; this is an intentional breaking change for direct import
clients, which must preview the archive first.

## Verification

The affected tests cover preview/import agreement, no database or audit-log
mutation, create/reuse/skip/reject outcomes, duplicate selectors/rules,
unsupported sections, source namespace remapping, blocking validation,
truncation, malformed archives, ZIP limits, entry-order determinism, drift
tokens and busy responses. The affected tests pass in the `shenyu-admin` reactor
on H2. A clean aggregate excluding the pre-existing timing-sensitive
`ShenyuClusterServiceTest` passed with 2726 tests, 0 failures and 12 skipped;
that class passes in isolation and is unrelated to this feature.

Live HTTP verification was also performed against Admin on the H2 profile:

- a clean archive previewed as `create=4, reuse=1, skip=1, reject=0` with
  `fullyValidated=true`;
- importing with that token succeeded with one new plugin, one new namespace
  relation, one selector and one rule;
- repeating the import with the old token returned `code=409`;
- previewing again after import returned
  `create=0, reuse=3, skip=3, reject=0` with `fullyValidated=true`;
- a mixed archive with `auth.json`, `plugin.json` and an orphan rule reported
  `SECTION_NOT_SUPPORTED`, `UNKNOWN_SECTION` and `MISSING_SELECTOR_REFERENCE`,
  set `fullyValidated=false`, and did not echo the fixture secret;
- missing authentication token returned HTTP 401; missing `previewToken`
  returned body `code=400`; unknown namespace, missing file and malformed ZIP
  returned body `code=400`.

Only H2 has been executed. MySQL, PostgreSQL, openGauss and Oracle were not run
for this feature and are not claimed as verified.

## Known boundaries

- Preview validates the source identity and the top-level fields used by the
  import, including plugin `role`/`enabled`, selector and rule required fields
  and their condition lists. It does not validate every database constraint,
  such as condition-row column lengths or handle column limits; those can still
  fail during the real import.
- Unsupported sections are not validated and still execute during the real
  import. `fullyValidated=false` is advisory; it does not block import.
  `blocking` is only meaningful for supported sections that had import-level
  validation; an unsupported section is reported with `supported=false` and may
  still fail later.
- The outcome budget bounds reported decisions, not the initial deserialization
  of a section. A section close to `max-entry-size` can still allocate a large
  object graph; the archive entry-size limit remains the outer memory bound.
- Duplicate existing namespace-plugin relation rows are tolerated by both
  preview and import using the first row, so dirty data does not abort the
  namespace-plugin section in the same way it used to.
- The concurrency semaphore is per instance, not cluster-wide.
- The preview token has no TTL or revocation. During a rolling upgrade, older
  Admin instances do not expose the preview endpoint and ignore the extra
  `previewToken` form field, so they still import without the required check.
  Token enforcement should be relied on only after all instances are upgraded.
- There is no preview history, persisted report or Dashboard integration. The
  external Admin Dashboard import flow must call `/configs/import/preview` and
  pass the token before importing; this repository does not update that UI.
- There is no database migration and no new persisted entity.

## References

- Feature issue: apache/shenyu#8192
- Transactional revisions and activation: apache/shenyu#8060
- Controlled fleet rollout: apache/shenyu#8193
- Configuration export: apache/shenyu#8301, apache/shenyu#8296
- Namespace import transaction bug: apache/shenyu#6620
- Acknowledgements and rollout diagnostics: apache/shenyu#7719, apache/shenyu#8066
- Authorization evolution: apache/shenyu#8177, apache/shenyu#8178
- Kubernetes/GitOps ownership: apache/shenyu#8070
