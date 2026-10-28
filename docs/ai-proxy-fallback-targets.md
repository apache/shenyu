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

# Request-selected AI fallback targets

AI Proxy accepts request-selected fallback only when the matched selector explicitly enables it. The request can select a target name from that selector's `fallbackTargets`; provider, endpoint, credential, model and other upstream settings come from the administrator-owned target. A target is authorized for every caller already admitted to that selector, so use separate selectors when callers need different target grants.

## Selector configuration

Add these fields to the AI Proxy selector handle through the Admin configuration API:

```json
{
  "provider": "OpenAI",
  "baseUrl": "https://primary.example/v1",
  "apiKey": "PRIMARY_KEY_PLACEHOLDER",
  "model": "primary-model",
  "dynamicFallbackEnabled": true,
  "fallbackTargets": {
    "backup": {
      "provider": "OpenAI",
      "baseUrl": "https://backup.example/v1",
      "apiKey": "BACKUP_KEY_PLACEHOLDER",
      "model": "backup-model"
    },
    "same-account": {
      "model": "alternate-model"
    }
  }
}
```

A request then uses only the target reference:

```json
{
  "messages": [{"role": "user", "content": "Synthetic example"}],
  "fallbackConfig": {"target": "backup"}
}
```

Target names contain 1–64 ASCII letters, digits, dots, underscores or hyphens and start with a letter or digit. Extra fields in `fallbackConfig`, unknown targets, null/non-object selections, and disabled selection return HTTP 400 before opening an upstream connection. Invalid selection does not silently switch to the Admin fallback or the primary target. Request JSON with duplicate fields or trailing values is rejected too.

`dynamicFallbackEnabled` defaults to false. Omitting `fallbackConfig` preserves normal primary and administrator-configured fallback behavior. An authorized request-selected target takes precedence over the administrator fallback. The existing retry, replay-safety, streaming and response-commit rules still decide when fallback can execute; authorization does not make a request replay-safe.

## Credential binding

A managed target inherits omitted model and generation settings from the resolved primary configuration. It may inherit the primary credential only when its provider identifier and base endpoint identity are unchanged. Endpoint identity compares scheme and host case-insensitively, explicit/default port equivalently, and the raw base path exactly. Provider identifiers are compared conservatively without alias expansion. The resolved fallback must have an explicit provider and base URL; missing identity fields fail closed instead of assuming a provider default. Query strings, URI credentials, fragments and non-HTTP(S) endpoints are rejected.

A changed provider, scheme, host, port or base path requires an explicit target `apiKey`. This rule also applies to administrator-configured fallback: a destination change does not implicitly copy the primary key. An explicit empty credential selects no credential and is only useful with a provider that supports unauthenticated requests; authenticated providers retain their own validation.

Proxy API keys are resolved per request before binding. A same-identity target without its own key uses that request's resolved key; a target with an explicit key uses only its configured key. Neither operation mutates selector defaults or another target.

Each request uses the selector configuration it captures when execution starts. Updates apply to later requests; they do not revoke a selector snapshot held by an in-flight exchange.

## Migrating inline fallback requests

* Move each trusted inline fallback definition into the appropriate selector's `fallbackTargets` and provide its own key when its upstream identity differs.
* Enable `dynamicFallbackEnabled` only for selectors whose admitted callers may choose those targets.
* Replace request `fallbackConfig` objects containing provider, URL, key or generation fields with `{ "target": "name" }`. There is no allow-any inline compatibility mode.
* If clients should not select fallback, remove their `fallbackConfig` field and keep selection disabled. Existing Admin fallback can remain configured through the normal fallback fields.
* Check old Admin fallback configurations that changed destination while omitting a key; configure an explicit target credential or retain the same upstream identity.
* Custom callers of the two-argument `resolveDynamicFallbackConfig` method have no selector authorization and cannot enable selection. Pass the matched trusted selector to the three-argument overload.

## Destination policy boundary

Managed target authorization controls which administrator-configured URI a request can select. It does not resolve or pin DNS addresses, impose an IP-range policy, or turn a hostname into a DNS-rebinding defense. Keep deployment egress and DNS restrictions in place for approved endpoints, and keep redirects disabled in the shared HTTP transport. The standard WebClient transport does not follow redirects; a custom transport must preserve that boundary. This change does not replace ShenYu's shared transport policy or make arbitrary third-party endpoints trusted.

Fallback resolution logs neither configuration objects nor request payloads. Rejections use fixed messages and the gateway records only the exception class before returning an empty error response.
