# AI RequestId Plugin

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

## Overview

The `aiRequestId` plugin establishes one opaque gateway request id for matched LLM
traffic before AI governance and upstream stages. The id is installed in the
request exchange and Reactor context, is reused by Agent Gateway LLM rules and
AI Proxy, and can be returned before response commitment. Top-level MCP traffic
is not covered in the first delivery; Agent Gateway MCP rules keep their own
entry id.

The plugin does not read or rewrite request/response bodies, does not create an
`AiRequestObservation`, does not use a global map or `ThreadLocal`, and does not
accept caller-supplied ids in the first version.

## Configuration

- Plugin name: `aiRequestId`
- Default sort: `168` (before Agent Gateway `169`)
- Rule handle: `{"responseHeaderEnabled": true}`; defaults to `true`
- Canonical header: `X-Shenyu-Ai-Request-Id`
- Legacy Agent Gateway header: `X-Shenyu-Agent-Request-Id` (controlled only by
  Agent Gateway's `responseRequestId`)

The plugin is seeded disabled in Admin. Unmatched requests keep their existing
behavior.

## More documentation

See [`docs/ai-request-id.md`](../../../docs/ai-request-id.md) for ordering,
header, CORS, logging, upgrade and non-goal details.
