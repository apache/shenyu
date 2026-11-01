<!--
  Licensed to the Apache Software Foundation (ASF) under one or more
  contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
  The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

      https://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-->

# Divide sticky session affinity and its cookie scope

The Divide plugin can keep a client on the upstream that first served it, as
described in #8037. The binding travels in a gateway-issued, HMAC-signed
cookie. The load-balancing algorithm still selects the target for a new
session; the affinity layer only reuses a previously selected target while it
stays inside the admitted candidate set (canary partition, health and circuit
breaker).

## Default storage is per rule

When a rule leaves `cookieName` at its default, `Shenyu-Route`, that value is
treated as a base name and the effective cookie name is derived from the rule
ID:

```text
Shenyu-Route-<12 hex characters of SHA-256(ruleId)>
```

Every rule therefore owns its own browser cookie, even when several rules are
served under the same host. Alternating traffic between such rules no longer
overwrites an established binding: visiting rule B leaves rule A's cookie in
place, so returning to A still resolves A's original target. This holds under
both the `REBIND` and `STRICT` policies.

The suffix is a digest rather than the raw rule ID because a cookie name may
only carry a restricted character set; hashing keeps arbitrary IDs valid and
bounds the name length.

## Explicitly configured names

Setting `cookieName` to any other value uses that name verbatim, which keeps
existing deployments that already pin a name working unchanged. Two rules that
configure the same name share one browser cookie on the same host and path:

- The last response to be written wins, so interleaved traffic between those
  rules replaces the stored binding, exactly as before #8136.
- The signed payload still carries the rule ID. A shared cookie written by one
  rule is a miss for the other, so a foreign binding is never honored; the
  second rule starts a new session instead.
- Under `STRICT` a miss allocates a new session rather than rejecting the
  request, because no valid binding was presented for that rule.

Operators who want isolation while keeping a recognizable name should give each
rule a distinct explicit name (for example `Shenyu-Route-orders` and
`Shenyu-Route-cart`), or keep the default name so each rule gets its derived
name. A blank `cookieName` disables affinity rather than selecting a name.

Because the cookie is written with `Path=/`, two rules sharing one explicit name
also share the cookie across paths of the host. A distinct explicit name per
rule is the way to separate them.

## Upgrades

The derived name changes the cookie a rule writes, including rules whose stored
handle already contains the former default `"cookieName":"Shenyu-Route"`. After
upgrading, a client holding a cookie under the former shared `Shenyu-Route` name
simply presents no binding for the derived name, so the first request starts a
new session and the response writes the rule-scoped cookie. The stale shared
cookie is left in the browser until it expires; it is not read by any rule.
Cookies under an explicitly configured name are unaffected.

## Guarantees that do not change

- Signature verification over every configured key, so keys rotate by appending.
- Expiry bound to `maxAgeSeconds`, and rejection of expired payloads.
- Rule-ID scoping of the signed payload, so a cookie of another rule is a miss.
- Candidate admission: a binding is honored only when its target is still in the
  filtered candidate set.
