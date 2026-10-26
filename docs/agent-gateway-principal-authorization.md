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

# Trusted identity and tools authorization

Agent Gateway adapts existing successful authentication; it is not an OAuth
authorization server and does not authenticate a raw token a second time.
Choose **one** authority for this gateway: key-auth rule bindings (see
[API key configuration](agent-gateway-key-auth.md)), an application-authenticated
principal described here, or a custom `AgentMcpSecurityResolver`.
All adapters default to disabled. No identity means no MCP access.

## Application prerequisites

| Source | Accepted authenticated principal | Application responsibility |
| --- | --- | --- |
| `jwt` | `JwtAuthenticationToken` | Resource Server filter chain, signature/algorithm, time, issuer/audience, key rotation |
| `opaque-token` | `BearerTokenAuthentication` | Resource Server filter chain, trusted introspection endpoint, active/expiry/revocation policy, TLS and credentials |
| `oauth2-login` | `OAuth2AuthenticationToken` | Trusted client registration, authorization code/state, token exchange, trusted UserInfo or OIDC validation, session and CSRF |

Add the selected Spring Security dependencies to the application. They are
optional in this starter: JWT needs resource-server and jose; opaque tokens need
resource-server and the chosen introspector's dependencies; login needs oauth2-client.
The application owns `SecurityWebFilterChain`, authentication managers, decoders,
introspectors and sessions. This adapter creates none of them. The old ShenYu
OAuth2 forwarding plugin is not a Resource Server authentication result, and
claims copied into HTTP headers by another plugin are not a trusted principal.

## JWT or opaque-token configuration

```yaml
shenyu:
  plugins:
    agent:
      gateway:
        enabled: true
        security:
          principal:
            enabled: true
            source: jwt # or opaque-token
            issuer: https://identity.example.com
            audience: shenyu-mcp
            # Optional: used ONLY for an authenticated identity without tenant_id.
            # default-tenant: explicitly-assigned-service-tenant
            scope-mappings:
              - scope: inventory:read
                tool-grants: [inventory.lookup, warehouse.stock]
                tool-denials: [warehouse.delete]
```

Issuer and audience are required and compared exactly. Trusted token attributes
must include string `iss` and a collection of strings in `aud` (the shape exposed
by the tested Spring Security converters). `registration-id` is rejected for
these sources. The application must validate tokens before this adapter runs.

For an application-owned OAuth login, select `source: oauth2-login` and set
`registration-id: trusted-registration` instead of issuer/audience. The selected
registration is the authority boundary; issuer/audience options are rejected in
login mode rather than silently ignored. OIDC authentication remains owned by
the application's OIDC client. Keep CSRF enabled for cookie-authenticated MCP
POST requests and send the application's CSRF token; this plugin does not
exempt the endpoint. Agent clients using bearer credentials should use a Resource
Server source instead of assuming support for browser login.

## Attribute and permission contract

- `sub` must match the authenticated principal name exactly. No principal aliases
  or header fallback are chosen silently. `tenant_id` must be nonblank bounded
  text. Missing tenant may use the explicitly configured default; malformed
  tenant never falls back, and anonymous traffic never gets a default identity.
- `tool_grants` and `tool_denies` are optional collections of exact tool names.
  `scope` (space-separated string or string collection) or `scp` is mapped only
  through the deployment's explicit scope mappings. If both exist, their sets
  must agree; otherwise the identity is rejected. Unmapped scopes grant nothing.
  `ROLE_*` and `SCOPE_*` authorities are not independently merged as a second
  permission source. Login permissions come from trusted principal attributes,
  not an unrelated access-token cache.
- Effective tools = `(direct grants + mapped grants - direct denies - mapped
  denies) ∩ Agent Gateway rule.allowedTools`. Denial wins across all mappings.
  `tools/list` and `tools/call` use the existing same request snapshot. A denied
  tool is hidden from the list and rejected before execution. Remote names keep
  their exact `server.tool` prefix. No wildcard or implicit scope-to-tool match.
- Identity strings are at most 256 characters without surrounding whitespace or
  control characters; tool names use `[A-Za-z0-9_.-]{1,128}`. Each permission/scope
  collection and mapping list is capped at 1024. A scope string is capped at
  8192 characters and uses space separators, not tabs or duplicate spaces.
- The configured source accepts only its matching authenticated token type.
  Missing/malformed identity, wrong issuer/audience/registration, or a verified
  key-auth result together with a principal is rejected; no source overrides
  another. Multiple resolver beans fail startup even with `@Primary`. Both
  built-in adapters enabled together also fail startup. Unknown settings,
  duplicate mappings and missing selected dependencies fail startup.
- Policies and identities are copied, not read from mutable configuration during
  execution. Policy changes require restart. Rule/credential sync is unchanged;
  newly admitted requests use current rules while already admitted calls retain
  their captured policy. Cancellation does not grant permissions or undo effects.
- Keys, tokens, raw claims and request bodies are not logged by the adapter.
  Inbound identity is not copied into outbound MCP credentials.

## Verification and boundaries

Tests cover real signed JWT and HTTP introspection through Spring Security and
the public key-auth/Agent Gateway plugin chain, plus real authorization-code
exchange and UserInfo using a loopback identity provider. JSON/SSE tools listing,
execution, deny/intersection, forged attributes, concurrent tenants, conflicting
authentication and rule revocation are covered. Login tests preserve CSRF/state
validation. API key cancellation and in-flight snapshots reuse their existing
regressions. Fixtures own their local configuration caches and identity providers:
this is not proof of a production IdP deployment or an Admin/WebSocket deployment.

Prompts/resources permissions follow those capabilities separately. AI/MCP
terminal events, quotas, tracing propagation and audit remain separate work.
The existing JWT-only PR and this local complete candidate overlap; reconcile
their public interfaces against merged master before publishing, without changing
other PRs or silently depending on an unmerged branch.
