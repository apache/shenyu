<!--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements. See the NOTICE file
distributed with this work for additional information
regarding copyright ownership. The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the
specific language governing permissions and limitations
under the License.
-->

# AI Proxy Rule generation overrides

#8127 adds an explicit opt-in generation block to Rule.handle. Plugin defaults
and Selector settings remain the only owners of provider, protocol, target,
credentials, proxy-key authentication, fallback configuration and native body
patch policy. Valid legacy Rule root fields remain inert; a previously unused
root-level model/provider is not silently activated.

The audit found AI Proxy Plugin/Selector metadata but no Rule-scope (type=2)
AI Proxy metadata in the four H2, MySQL, PostgreSQL and Oracle initialization
scripts. Existing repository configuration/examples and the prior request path
resolve provider settings from Plugin/Selector data. This delivery keeps that
metadata intact and exposes the version-one RULE contract through the existing
configuration-contract API. Configure the new JSON block with the existing Admin
Rule API/import; no new dashboard form control or database migration is implied.

Example:

```json
{
  "generationOverrides": {
    "model": "operator-model",
    "temperature": null,
    "maxTokens": 1024,
    "stream": false
  }
}
```

| Field | Allowed value | Explicit null |
| --- | --- | --- |
| model | Nonblank string, at most 256 characters, no surrounding whitespace/control characters | Clear configured model; use the client model |
| temperature | Finite JSON number in [0,2] | Clear configured override; retain client/provider default |
| maxTokens | Positive JSON integer within Java int range | Clear configured override; retain client/provider default |
| stream | JSON boolean | Clear configured default; use explicit client value or non-streaming default |

Absent fields inherit Plugin/Selector configuration. An absent, null or empty
generationOverrides block means no Rule override. Plugin < Selector < Rule is
the configured precedence. The existing engine applies configured model,
temperature and maxTokens over client values; **explicit client stream remains
authoritative**. Null clearing removes the configured value rather than filling
it from the lower scope again. Removing the block restores lower-scope
inheritance. Provider dialect/model/token-field mappings and existing native
body-patch policy still run in their original order; Rule configuration does
not override those protected mechanisms.

Only these four nested keys are accepted. Credentials, provider, baseUrl,
protocol, fallbackConfig, proxyEnabled, requestBodyOverrides and any other
nested field are rejected, including null-valued attempts. Root legacy fields
remain ignored. New publication validation requires blank/null or a valid JSON
object; malformed legacy handles, arrays and duplicate keys should be replaced
with a valid object before editing/importing them. This is an explicit migration
constraint for previously unused arbitrary Rule payloads, not reinterpretation
of their fields.

The shared lightweight parser/contract validates Admin create/update/import and
gateway publication with bounded, value-free errors. It limits source length to
65,536 characters and JSON depth to 32. No network, client initialization or
credential resolution occurs in validation. The gateway parses once on a Rule
update and publishes an immutable value. Invalid updates retain the previous
published value. Rule deletion (even with only its ID), Selector removal and
Plugin removal evict owned values. Rule-ID reuse is also checked against the
actual selected Selector identity.

The existing request owner reads one cached value and merges the four fields
into its request-owned AiCommonConfig. It does not parse Rule JSON per request,
modify cached DTOs, or share a mutable effective configuration with another
request. The normal engine/provider capability and request validation runs
before transport execution. Missing required effective target/model settings
still fail there; Rule overrides do not grant model authorization or add virtual
model policy.

Fallback selection, accepted request-body fallback configuration, credential
supplementation and no-replay-after-output remain with their existing owners.
They are not new Rule fields. An existing fallback resolver may inherit
generation values from the already-resolved primary configuration where it did
so before; an explicitly configured fallback model still takes precedence.
No alternate fallback mechanism or credential inheritance rule is introduced.

AiProxyRuleContractTest covers presence, clearing, strict types, bounds,
protected fields, duplicate keys and immutable publication. RuleServiceTest
exercises real Admin create/update/import validation before writes/events.
AiProxyPluginTest exercises the real plugin and engine through outgoing requests,
including provider token-field adaptation, credential preservation, stream
precedence, clearing and invalid-update/removal behavior.
AiProxyConfigServiceTest checks all three scopes, request ownership and
concurrent independent resolution. Existing Selector-only, proxy-key and
fallback tests remain compatibility controls.
