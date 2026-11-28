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

# HTTP tool identity delegation

[Feature request and acceptance criteria](https://github.com/apache/shenyu/issues/8392)

## Requirements

A configured HTTP tool may name an identity delegation policy for its fixed
route. Admission requires the rule and the authenticated identity to grant the
tool. The gateway must use the captured tenant and subject from its trusted
security resolver. Only the backend can grant access to an individual record.

- **When the tool omits delegation**, the gateway must dispatch an isolated
  child without caller credentials, cookies, principal or web session.
- **When a tool requests delegation**, Admin and the gateway must validate
  its audience, ordered unique scopes, key reference/version, TTL and tenant
  requirement. Acquisition must fail closed if no delegator is available.
- **When the built-in signer issues an assertion**, it must load the current
  versioned Ed25519 key, verify its status and exact tool/method/path/audience
  binding, and require every requested scope to be allowed. Each assertion
  must have a fresh ID and must expire by the earliest of TTL, invocation
  deadline and key expiry. A tenant is required by default.
- **When routing a delegated child**, the authorization header must carry only
  its issued bearer. The gateway must check the admitted route, selected
  upstream authority, HTTP method and final request before sending. Ordinary
  route authentication and authorization still apply.
- **When acquisition fails, is cancelled, exceeds its deadline, or a pending
  invocation loses its binding**, the gateway must fail before business
  dispatch. A delegated invocation must not replay a business call.
- **When the rule or key changes**, subsequent invocations must use the current
  binding. Removal or revocation prevents further issuance. Backends verify
  already-issued assertions against their own public-key trust and expiry.
- **When reporting delegation failures**, metrics and client diagnostics must
  use bounded classifications without bearer tokens, keys or raw identities.
  A deadline failure uses HTTP 504 and JSON-RPC -32025.
- **When invoking a remote MCP tool**, the fixed-service credential contract
  continues to apply. Any future delegation adapter requires its own explicit
  target binding.
