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

# MCP contract evolution regression fixtures (#7935)

Synthetic, non-executable single-page `tools/list` fixtures. Seven JSON files are
copied verbatim from [ChangeProof's public pilot](https://github.com/jajazpua/ChangeProof/tree/11bb0c33d70e0211e5649c27566da7069c0f0824/pilots/shenyu-7935)
(commit `11bb0c33d70e0211e5649c27566da7069c0f0824`, November 1, 2026).
This is not a live ShenYu capture. The original independent Python fixture
verifier passed 38 checks when run by @Aias00; this new Java test is separate.

## Cases

- `combined`: an existing optional `workspace_id` input becomes required
  (old requests may fail); output `status` enum expands to `paused`
  (potential break **only for strict/exhaustive old consumers**); and
  `readOnlyHint` changes (review metadata, not automatic schema break).
- `annotation_only`: schemas unchanged, annotation drift requires review.
- `key_order_only`: equivalent parsed JSON objects have no structural drift.
  This does not claim RFC 8785 canonicalization or semantic equivalence.
- `unsupported`: `allOf`/`not` composition must be `REQUIRES_REVIEW`,
  never an unconditional compatibility PASS.

The original `expected/structural_changes.json` gives literal RFC 6901
paths; `expected/compatibility.json` records directional expectations and
four request/output instances. The JUnit test checks exact structural differences,
instances with a deliberately narrow local evaluator, and review classifications.
The evaluator is **not a general JSON Schema validator** and rejects composition.

## Run

From the ShenYu repository root:

```sh
./mvnw -pl :shenyu-plugin-mcp-server -am -Dtest=McpContractRegressionFixtureTest -Dsurefire.failIfNoSpecifiedTests=false test
```

On Windows PowerShell, use `./mvnw.cmd` in place of `./mvnw`.

No proprietary ChangeProof code, external CLI, credentials, live MCP calls, or
production state is required. This fixture does not test runtime pinning,
approvals, generation consistency, permissions, invocation semantics, rollback,
or arbitrary JSON Schema equivalence. Those are independent #7935 criteria.
