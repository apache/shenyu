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

# JWT authentication challenges for MCP

A request matched by the JWT plugin's selector and rule must provide a valid
JWT. Missing, malformed, expired or invalid credentials return **HTTP 401**
and a `WWW-Authenticate: Bearer` challenge, while retaining the existing
ShenYu error body. This also applies to ordinary HTTP routes protected by JWT;
clients must no longer rely on HTTP 200 for authentication errors.
Valid credentials and the legacy `token` header keep their existing behavior.

Authentication must run before the MCP transport opens an SSE stream or handles
a Streamable HTTP message. Enable JWT and match **all** protected MCP request
paths, including `/sse`, the SSE message endpoint, and `/streamablehttp` under
any configured server prefix. Use the actual deployment paths when configuring
selectors and rules. Merely enabling the MCP plugin does not require
credentials on otherwise public routes.

## Advertising protected resource metadata

To let an OAuth MCP client discover the authorization server, optionally add
`resourceMetadataUrl` to the JWT plugin's **plugin-level JSON config** in Admin:

```json
{
  "secretKey": "<existing-verification-key>",
  "handleType": "default",
  "resourceMetadataUrl": "https://gateway.example/.well-known/oauth-protected-resource/mcp"
}
```

The authentication rejection then includes:

```http
HTTP/1.1 401 Unauthorized
WWW-Authenticate: Bearer resource_metadata="https://gateway.example/.well-known/oauth-protected-resource/mcp"
```

`resourceMetadataUrl` defaults to unset; omitting it or setting it to an empty
string removes the metadata parameter. It must be an absolute HTTP(S) URL
without user information or a fragment. Malformed URLs are rejected before
replacing the active JWT configuration. Use HTTPS in production. The address
is explicitly configured, not derived from untrusted request or forwarding
headers, and the plugin never fetches it. An existing `WWW-Authenticate`
response header supplied by the deployment is preserved.

The deployment must serve the public metadata URL **without JWT authentication**.
Its [RFC 9728](https://www.rfc-editor.org/rfc/rfc9728) document identifies the
protected resource and at least one authorization server, for example:

```json
{
  "resource": "https://gateway.example/mcp",
  "authorization_servers": ["https://identity.example"],
  "bearer_methods_supported": ["header"]
}
```

The authorization server provides its discovery document, authorization and
token endpoints, and Dynamic Client Registration when supported. If cross-origin
browser clients are used, the deployment's CORS policy must expose
`WWW-Authenticate` on authentication errors. No credentials, requested scopes
or token parsing errors are echoed in the challenge.

## Boundaries

This fixes the HTTP authentication challenge; it does not implement an OAuth
authorization server, DCR, a metadata endpoint or a new token verification
strategy. Configure the verifier appropriate for the issuer and resource,
including audience validation. The existing `oauth2` plugin forwards tokens
from an authenticated OAuth login; it is not a Bearer resource-server verifier.
For application-owned Spring Security resource servers, configure their own
Bearer authentication entry point and protected-resource metadata challenge;
this JWT setting does not replace a `SecurityWebFilterChain`.

See the [MCP authorization specification](https://modelcontextprotocol.io/specification/2025-06-18/basic/authorization)
for the discovery flow. Insufficient permissions remain an authorization
failure (403); this change only handles failures to authenticate a JWT.
