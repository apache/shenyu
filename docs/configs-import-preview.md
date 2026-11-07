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
entity. Preview is additive: the existing import endpoint remains backward
compatible, accepts an optional preview token, and never blocks on a missing or
stale token.

## API

### Preview request

```http
POST /configs/import/preview
Content-Type: multipart/form-data
Permission: system:manager:importConfig
```

| Field | Required | Description |
| --- | --- | --- |
| `namespace` | yes | Target namespace. The namespace must exist. |
| `file` | yes | Import archive. Must be a valid ZIP within the configured limits. |

The response is a `ShenyuAdminResult` whose `data` is the preview report. Preview
uses the same permission and namespace check as the normal import. Authentication
failures are handled by the existing Shiro filter and return HTTP 401.

Preview errors use the established Admin body-code convention:

| Body code | Meaning |
| --- | --- |
| `400` | Missing parameters, unknown namespace, malformed or oversized archive |
| `429` | Preview concurrency limit reached |
| `500` | Unexpected failure, with a generic message |

### Import request

```http
POST /configs/import
Content-Type: multipart/form-data
Permission: system:manager:importConfig
```

| Field | Required | Description |
| --- | --- | --- |
| `namespace` | yes | Target namespace. The namespace must exist. |
| `file` | yes | Import archive. |
| `previewToken` | no | Token returned by a previous preview. |

The import endpoint remains backward compatible. `previewToken` is advisory: if
it is omitted, stale, or cannot be checked, the import proceeds and returns a
`PREVIEW_TOKEN_WARNING` object in `data`. The warning is also logged. A token is
never required, and a stale token never blocks the import.

```json
{
  "PREVIEW_TOKEN_WARNING": {
    "code": "PREVIEW_TOKEN_STALE",
    "message": "import proceeded because the archive or target state changed since preview; drift was not checked"
  }
}
```

Warning codes:

| Code | Meaning |
| --- | --- |
| `PREVIEW_TOKEN_MISSING` | No token was supplied |
| `PREVIEW_TOKEN_STALE` | Token does not match the current archive or baseline |
| `PREVIEW_TOKEN_UNVERIFIABLE` | Token could not be checked, for example due to an oversized archive, a busy preview budget or an internal validation failure |

Callers that need a current guarantee re-run preview immediately before applying.

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
    {"code": "OBSERVATION_ONLY", "message": "..."},
    {"code": "REVALIDATE_BEFORE_IMPORT", "message": "..."}
  ]
}
```

Preview warnings use the same `{code, message}` shape as the import
`PREVIEW_TOKEN_WARNING`.

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
example a required-field validation failure, a rule handle validation failure or
malformed section JSON. Per-object outcomes remain diagnostic; the live import
is fail-fast.

`truncated` means the outcome budget stopped planning. Later sections are marked
`REPORT_TRUNCATED` and are not fully analyzed.

### Baseline and drift

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
fingerprint. It is stateless and has no TTL. Missing or stale tokens follow the
warning behavior described under Import request. If another writer changes the
archive or target state between preview and import, the report is stale; callers
that need a current guarantee should re-run preview immediately before applying.
Import sections retain their existing transaction boundaries, so no atomicity
claim is made.
