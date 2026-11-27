# Protect JSON bodies with JWE Compact

This draft provides a separately installable `shenyu-plugin-jwe` gateway module for selected HTTP Divide routes. It operates on the whole document and does not change the field-level Cryptor plugins. The community can review the module or Plugin Store placement independently of the wire profile.

## Enable a route

Add the module to the gateway application's dependencies, then declare a bean in a Spring configuration class scanned by the gateway. Supply keys through your deployment's secret/key management system. The map key is the **ShenYu rule ID**.

```java
@Bean
JwePayloadPlugin jwePayloadPlugin(RSAPrivateKey gatewayDecryptionKey,
                                  RSAPublicKey partnerRecipientKey) {
    return new JwePayloadPlugin(Map.of("partner-echo-rule-id",
            new JweRoute(gatewayDecryptionKey, partnerRecipientKey)));
}
```

Register and enable `jwePayload` in Admin, create a selector and rule for the partner's POST route, and use the same rule ID as the deployment. Configure an order before Divide and the upstream response writer (suggested: 99). A matched but unconfigured rule fails closed. Scope the selector to the intended routes and test on every gateway: an unmatched route is deliberately untouched. There is no new default plugin row, Admin key field or automatic key discovery.

Use separate RSA keys of at least 2048 bits. The client encrypts to the gateway public key; the gateway encrypts responses to the trusted recipient public key for the matched rule. Use a separate authenticated route/rule per recipient. JWE alone does not authenticate a caller. Supplying `null` as the first or second constructor argument disables request decryption or response encryption respectively; at least one direction must be configured.

## Request and response

With both directions enabled, send a compact JWE as `Content-Type: application/jose` to the configured POST route. Its protected header includes `{"alg":"RSA-OAEP-256","enc":"A256GCM","cty":"application/json"}`. The forwarded request contains the original UTF-8 JSON bytes with `application/json`. An upstream JSON response is encrypted for the configured recipient, returned with its original status, `application/jose`, and the encrypted length. For an encrypt-only route the original request must itself be JSON.

The input wire body and the upstream clear response are each limited to 1 MiB. Do not use content encoding, JWE compression or streaming responses on these routes. Configure upstreams to return ordinary JSON bodies. Unsupported request media/methods, malformed ciphertext and oversize input are rejected before forwarding. If the upstream representation cannot be encrypted, the gateway returns an empty 502 rather than upstream plaintext. No form, GET query or WebSocket transformation is included. Key provisioning, rotation and recipient authorization remain deployment responsibilities.
