# Protect JSON bodies with JWE Compact

This draft provides a separately installable `shenyu-plugin-jwe` gateway module for selected HTTP Divide routes. It operates on the whole document and does not change the field-level Cryptor plugins. The community can review the module or Plugin Store placement independently of the wire profile.

## Enable a route

Add `org.apache.shenyu:shenyu-spring-boot-starter-plugin-jwe` to the gateway. Supply RSA private/public key beans through trusted local configuration or a secret provider. The starter resolves the bean names at startup. HTTP data cannot select a Spring bean or an external key location.

```yaml
shenyu:
  jwe-payload:
    max-ciphertext-bytes: 1048576
    max-plaintext-bytes: 524288
    routes:
      - namespace-id: partner-namespace-id
        rule-id: partner-echo-rule-id
        decryption-keys:
          old: gatewayOldPrivateKey
          current: gatewayCurrentPrivateKey
        recipient-key-id: partner-current
        recipient-key-bean: partnerRecipientPublicKey
```

Register and enable `jwePayload` in Admin and configure the partner's POST selector and rule. Trust lookup requires the exact synchronized **namespace ID and rule ID**. A missing namespace or policy fails closed; a request cannot fall back to a different tenant's keys. There is no automatic Admin provisioning. As with other selector-based plugins, unmatched routes are untouched.

Use separate RSA keys of at least 2048 bits. The client encrypts to the gateway public key; the gateway encrypts responses only to the recipient pinned for the matched rule. Use a separate authenticated route per recipient. JWE alone does not authenticate a caller. Omit `decryption-keys` for encrypt-only, or omit both recipient settings for decrypt-only. At least one direction is required.

### Rotate keys

For inbound rotation, add the new protected `kid` alongside the old one in that route's key ring, roll out the configuration to every gateway, then switch clients to the new public key. Retire the old ID on all gateways after in-flight deliveries and retries have drained. Unknown, missing and retired IDs are rejected; no other key is tried. For outbound rotation, install the new private key at the client first, then switch the configured recipient ID and public key on the gateways. The client should accept both recipient IDs during the rollout. Key sets are immutable until restart/redeployment.

## Request and response

With both directions enabled, send a compact JWE as `Content-Type: application/jose`. Its protected header includes `{"alg":"RSA-OAEP-256","enc":"A256GCM","cty":"application/json","kid":"current"}`. Forwarding restores the original UTF-8 JSON bytes with `application/json`. A JSON response retains the upstream status and is returned with `application/jose`, the configured recipient ID in its protected header, and the encrypted length. An encrypt-only route expects JSON on ingress.

The ciphertext and plaintext limits each default to 1 MiB, apply in both directions and must be positive. The starter honors a smaller explicitly configured gateway codec ceiling (`spring.http.codecs.max-in-memory-size`, or the legacy `spring.codec.max-in-memory-size`). Decrypted bytes are size-checked before JSON parsing. Disabled JWE compression prevents compressed data from expanding during decryption. The normal HTTP client's retry-cache limit still applies.

Run JWE after verification of the received ciphertext, before plaintext body transformers, Divide, the HTTP retry cache and the response writer (suggested order 99). The client then caches the decorated plaintext and recalculated length for retries. If an HTTP retry cache already exists when decryption is requested, reject instead of forwarding stale ciphertext. Configure matching selectors consistently on all gateways.

An unsupported method returns 405; unsupported media or content encoding returns 415. Invalid envelopes, keys or JSON return 400; oversized input returns 413. Missing route trust or an incompatible cache order returns 500. If a response cannot be encrypted (including oversized or streaming output), return an empty 502 without upstream body bytes. An upstream error with a supported JSON body retains its status **and** gets encrypted. Forms, GET queries and WebSockets are outside this profile.
