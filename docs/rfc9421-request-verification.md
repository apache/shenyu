# Verify signed HTTP requests

This draft adds an optional gateway module, `shenyu-plugin-message-signature`. It does not change the protocol of the existing `sign` plugin. The separate plugin placement is proposed for review.

## Enable a route

Add the module to the gateway application's dependencies. In a configuration class scanned by that application, register a bean like the following. Supply the public key through trusted deployment configuration. The map key is the **ShenYu rule ID**, not the HTTP signature's key ID.

```java
@Bean
MessageSignaturePlugin messageSignaturePlugin(PublicKey partnerVerificationKey,
                                             ReactiveStringRedisTemplate sharedRedis) {
    NonceStore nonces = new RedisNonceStore(sharedRedis);
    MessageSignatureVerifier verifier =
            new MessageSignatureVerifier("partner-2026", partnerVerificationKey, nonces);
    return new MessageSignaturePlugin(Map.of("verified-webhook-rule-id", verifier));
}
```

In Admin, add and enable the plugin `messageSignature`, its selector and rule for the HTTP route, using the same rule ID as the deployment. Place it **before** any plugin that rewrites the request target or body (the suggested order is 19). No automatic registration, schema migration, starter or private key distribution is included. A matched rule without a verifier is denied. Like other selector-based plugins, unmatched routes continue normally; activate the policy on all gateways before exposing a protected route.

For replay protection, configure all gateway instances with the same Redis trust domain, key IDs and synchronized clocks. Supply Spring Data Redis and its reactive driver in that deployment; for verification without nonce protection pass `null` instead of a store. A local, per-instance cache does not provide distributed replay protection. Use a stable, unique key ID for each verification key.

## Wire profile

* Send one `Signature-Input` and one `Signature` header with label `sig1`. The algorithm is `ed25519`. Use exactly the unparameterized components `@method` and `@target-uri`, plus `content-type` and `content-digest` for a request with a body or either representation header. The order is the sender's signed order.
* Provide signed `keyid`, `alg`, `created` and `expires`; creation must not be in the future, expiry must be in the future and no more than 300 seconds after creation. When replay protection is enabled also provide a nonblank signed `nonce` of at most 128 characters.
* Use a single RFC 9530 `Content-Digest: sha-256=:...:` for the original body bytes. This bounded profile accepts up to 1 MiB. It uses the absolute target URI seen by the gateway; trusted proxy configuration must preserve the public target. Do not accept arbitrary forwarded host overrides from callers.
* Example `Signature-Input` parameter string (substitute current epoch seconds when signing):
  `sig1=("@method" "@target-uri" "content-type" "content-digest");created=1700000000;expires=1700000060;keyid="partner-2026";alg="ed25519";nonce="delivery-123"`.

Invalid signatures or repeated nonces return 401. Body overflow returns 413. If nonce storage fails, the gateway returns 503 without forwarding. The forwarded request body is the verified byte sequence. This module verifies possession of the configured signing key; a deployment must separately bind that key to an authorized caller and route. It does not sign responses or provide a general-purpose implementation of every RFC 9421 profile.
