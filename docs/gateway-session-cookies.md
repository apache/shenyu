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

# Gateway and backend session cookies

The packaged `shenyu-bootstrap` configuration uses `SHENYU_SESSION` for the
gateway's reactive WebSession cookie. Backend applications can continue using
`SESSION` without sharing the gateway's session identifier.

Spring WebFlux can expire an incoming cookie when a component accesses a session
identifier that is unknown to the gateway's session store. Security filters may
access the session independently of the dashboard's OAuth2 plugin enabled flag.
If both gateway and backend use `SESSION`, the resulting `Set-Cookie: SESSION=;
Max-Age=0` can clear the backend's browser login. The gateway and backend need
different session-cookie names on a shared public origin.

## Configuration and scope

The default is set using Spring Boot's standard reactive session property:

```yaml
server:
  reactive:
    session:
      cookie:
        name: SHENYU_SESSION
```

The bootstrap distribution copies this same application configuration into its
`conf` directory. Custom launchers or external configuration files that replace
the packaged configuration should set this property explicitly. For example, an
operator can select another unused name with
`SERVER_REACTIVE_SESSION_COOKIE_NAME=CUSTOM_GATEWAY_SESSION`.

This configures the gateway's own WebSession identifier. It does not rename
proxied backend cookies or change OAuth2 authorization rules. Spring Boot's
existing cookie path, domain, Secure, HttpOnly, SameSite, and session-store
configuration continue to apply.

When the gateway receives an expired `SHENYU_SESSION`, it may clear that cookie
while forwarding a backend `SESSION`. Choose a different gateway name if a
backend already uses `SHENYU_SESSION`.

## Upgrade compatibility

An existing gateway login saved under the former `SESSION` name will not be
recognized under the new default cookie name. Users may need to authenticate
with the gateway again after adopting this configuration. Backend session
identifiers and persisted session-store data are not rewritten.

An explicitly configured cookie name takes precedence over the packaged
configuration. A deployment deliberately retaining `SESSION` can set
`server.reactive.session.cookie.name=SESSION`; this also retains the original
collision behavior if a backend uses that name. Review both cookie owners
before choosing the compatibility override.

These defaults apply to the current packaged bootstrap configuration. An older
ShenYu deployment needs the property added to its own compatible Spring Boot
configuration; changing the dashboard plugin flag alone does not isolate
WebFlux cookies.

## Regression coverage

`GatewaySessionCookieTest` reads the actual bootstrap YAML and uses Spring Boot's
`WebSessionIdResolverAutoConfiguration`, `DefaultWebSessionManager`, a real
reactive HTTP handler, and an ephemeral loopback HTTP server. It checks backend
cookie preservation, expiry of only the gateway cookie, creation of new gateway
sessions, explicit name overrides, and a legacy shared-name collision control.

The fixture exercises the WebFlux session boundary without contacting a CAS or
OAuth2 identity provider. External provider login/logout and deployment-specific
cookie domain/path policies should also be checked when rolling out the change.
