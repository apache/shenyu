# Verify signed HTTP requests

This draft adds an optional gateway module, `shenyu-plugin-message-signature`. It does not change the protocol of the existing `sign` plugin. The separate plugin placement is proposed for review.

## Enable a route

Add `org.apache.shenyu:shenyu-spring-boot-starter-plugin-message-signature` to the gateway. Register the Ed25519 public key as a trusted Spring bean and bind it to a rule by name. The key registry is local to the deployment; no key material belongs in the Admin rule or HTTP headers.

```yaml
shenyu:
  message-signature:
    routes:
      - rule-id: verified-webhook-rule-id
        key-id: partner-2026
        public-key-bean: partnerVerificationKey
        replay-protection: true
```

For replay-protected routes supply a shared `NonceStore` bean. For example, with Spring Data Redis and the reactive driver in the deployment:

```java
@Bean
NonceStore nonceStore(ReactiveStringRedisTemplate sharedRedis) {
    return new RedisNonceStore(sharedRedis);
}
```

In Admin, add and enable the plugin `messageSignature`, its selector and rule for the HTTP route, using the same rule ID as the deployment. Place it **before** any plugin that rewrites the request target or body (suggested order 19, before Rewrite at 90). A matched rule without a verifier is denied. Unmatched routes continue normally; activate the policy on every gateway before exposing a protected route. The starter does not create Admin rows or distribute keys.

For replay protection, configure all instances with the same Redis trust domain, key IDs and synchronized clocks. A protected route returns 503 for an otherwise valid request if the store bean is absent, empty or unavailable; it never falls back to verification only. Routes with `replay-protection: false` (the default) require neither Redis nor a store. Each verification key must have a stable, unique key ID. For programmatic registration use the explicit factories `verificationOnly` and `replayProtected`.

## Wire profile

* Send one `Signature-Input` and one `Signature` header with label `sig1`. The algorithm is `ed25519`. Use exactly the unparameterized components `@method` and `@target-uri`, plus `content-type` and `content-digest` for a request with a body or either representation header. The order is the sender's signed order.
* Provide signed `keyid`, `alg`, `created` and `expires`; creation must not be in the future, expiry must be in the future and no more than 300 seconds after creation. When replay protection is enabled also provide a nonblank signed `nonce` of at most 128 characters.
* The single signed `@target-uri` covers the original scheme, authority, path and query. Separate authority/request-target components are not accepted by this first profile. An upstream rewrite after verification does not change which target the client signed.
* Use a single RFC 9530 `Content-Digest: sha-256=:...:` for the original body bytes. This bounded profile accepts up to 1 MiB. It uses the absolute target URI seen by the gateway; trusted proxy configuration must preserve the public target. Do not accept arbitrary forwarded host overrides from callers.
* Example `Signature-Input` parameter string (substitute current epoch seconds when signing):
  `sig1=("@method" "@target-uri" "content-type" "content-digest");created=1700000000;expires=1700000060;keyid="partner-2026";alg="ed25519";nonce="delivery-123"`.

Invalid signatures or repeated nonces return 401. Body overflow returns 413. If nonce storage fails, the gateway returns 503 without forwarding. The forwarded request body is the verified byte sequence. This module verifies possession of the configured signing key; a deployment must separately bind that key to an authorized caller and route. It does not sign responses or provide a general-purpose implementation of every RFC 9421 profile.
