# Password request encryption

Login and self-service password changes can use the same versioned public-key envelope.
This feature adds new MVC adapters; it does not replace the existing controllers, DTOs,
authentication services, authorization checks, password hashes or token invalidation.

## Public capability

`GET /platform/secretInfo` keeps its existing Admin response shape. The Base64 JSON in
`data` keeps `key: ""` and `iv: ""`, and adds:

```json
{
  "encryption": {
    "version": 1,
    "algorithm": "RSA-OAEP-256+A256GCM",
    "keyId": "<Base64url SHA-256 of SPKI, without padding>",
    "publicKey": "<standard Base64 DER SPKI>",
    "serverTime": 1790000000000,
    "maxAgeSeconds": 120
  }
}
```

The response uses `Cache-Control: no-store`. It contains no private key or configured
legacy AES key/IV. The underlying `SecretService` remains unchanged.

## Request contract

Only POST `/platform/login` (`LoginDashboardUserDTO`) and PUT
`/dashboardUser/modify-password/{id}` (`DashboardUserModifyPasswordDTO`) are adapted.
The entire body is exactly:

```json
{
  "version": 1,
  "keyId": "<advertised keyId>",
  "wrappedKey": "<standard Base64 RSA-OAEP ciphertext>",
  "iv": "<standard Base64 12-byte IV>",
  "ciphertext": "<standard Base64 AES-GCM ciphertext followed by 16-byte tag>"
}
```

The client generates a fresh 256-bit AES key and 12-byte IV for each request. RSA OAEP
uses SHA-256 **and MGF1-SHA-256**, an empty label, and the advertised RSA public key
(minimum 2048 bits). AES uses GCM with a 128-bit authentication tag. Additional
authenticated data is UTF-8:

```
shenyu-password/v1|<keyId>|<purpose>|<target>
```

`purpose` is `login` with an empty target, or `change-password` with the actual decoded
path ID. The decrypted JSON is exactly `{issuedAt, data}`, where `issuedAt` is the
`serverTime` obtained from the latest capability response and `data` contains the
original DTO fields. It is accepted for `max-age-seconds` (120 by default), with at
most 30 seconds future skew. Fetch fresh metadata before retrying an expired request.
Unknown/duplicate envelope fields, noncanonical Base64, wrong keys/tags/routes,
invalid JSON and bodies above 64 KiB are rejected. The original password complexity
and old-password checks still run. Login DTO validation runs before the legacy
compatibility bridge can transform a blank password into nonblank ciphertext.

For installations with valid legacy `shenyu.aes.secret.key` and `iv`, the adapter
reencrypts the authenticated plaintext login password internally with that legacy
AES/CBC configuration so the unchanged service decodes it exactly once. Both empty
settings permit internal plaintext passthrough. Partial or invalid configurations
fail closed. The client cannot select a flag that skips password processing.

Password transport/credential DTO validation errors return the existing HTTP 200 Admin
error shape with a generic message and contain no rejected values or underlying
exceptions. Unrelated DTO validation keeps the original field/default-message format;
unrelated JSON parse errors keep the original generic busy response. If parse handler
metadata is unavailable, the response is simply "Invalid request". These handlers do
not log submitted credentials, rejected values or validation exceptions.

## Compatibility and production configuration

The default `shenyu.password-encryption.required=false` preserves existing legacy
API clients. This default **does not prohibit plaintext legacy requests**. The new
UI requires the advertised envelope and never falls back to plaintext or old AES.
Enable the following policy to reject legacy JSON password requests and the legacy
GET/HEAD `/platform/login` route. OPTIONS preflight remains available:

```yaml
spring:
  config:
    import: optional:configtree:/run/shenyu-secrets/
shenyu:
  password-encryption:
    required: true
    max-age-seconds: 120
```

Generate a private key into the protected runtime secret directory, outside Git:

```sh
umask 077
openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:3072 \
  -out /run/shenyu-secrets/shenyu.password-encryption.private-key
```

The property accepts PKCS8 PEM or standard Base64 PKCS8 DER. Do not print or commit
this file. Persist the secret securely and mount the same private key on every Admin
instance; generated process-local keys are only suitable for a single development
instance and change on restart. Invalid/undersized keys fail initialization without
including the key or its decoding exception in the error. Supported request age is
1–3600 seconds. Do not make application logs, process environments or deployment
records a secret transport.

HTTPS remains required in production: application encryption does not authenticate
a downloaded public key or JavaScript over HTTP. AAD and timestamps bind a request
to an operation and limit its age; this is **not full replay prevention** within the
accepted window. Existing session and authorization checks remain necessary.

### Version combinations and rollout order

| Admin | Dashboard or API client | `required` | Credential requests |
| --- | --- | --- | --- |
| Without envelope support | Legacy client | Not applicable | Existing contract |
| Without envelope support | Envelope dashboard | Not applicable | Unsupported; submission stops when public capability is missing |
| With envelope support | Legacy client | `false` | Existing legacy contract remains available |
| With envelope support | Legacy client | `true` | Legacy credential requests are rejected |
| With envelope support | Envelope dashboard | `false` or `true` | Envelopes are accepted, subject to the original credential checks |

1. Deploy the envelope-capable Admin to every instance that can receive metadata
   or credential requests, using the same persistent private key and production
   HTTPS. Keep `required=false` during migration and verify each instance advertises
   the same `keyId`; a mixed legacy/envelope Admin pool is not supported by the new UI.
2. Deploy the envelope dashboard, then verify login and self-service password changes.
   Migrate other legacy credential clients before requiring envelopes.
3. Enable `required=true` on every Admin only after migration is complete. Apply
   configuration changes with the necessary instance restarts and verify the policy
   on every instance before treating legacy requests as disabled.

### Coordinated key rotation

This implementation loads one private key at startup and accepts only its current
`keyId`. It has no key ring, previous-key grace window or live secret-file reload.
Do not expose old-key and new-key instances to the same credential traffic during
a rolling key change. If uninterrupted rotation is required, a separate design for
overlapping keys is needed; the following procedure uses a maintenance window.

1. At the ingress, pause new `/platform/secretInfo`, `/platform/login` and
   `/dashboardUser/modify-password/{id}` traffic to all Admin instances. Stop issuing
   old-key metadata before beginning the drain.
2. Drain requests already accepted. With synchronized clocks, wait at least the
   largest configured `max-age-seconds` plus the 30-second future-skew allowance
   after the last old-key metadata response; add any known clock uncertainty.
   This drains ordinary issued envelopes, not every client-side queued request.
   An old envelope arriving after cutover is rejected and must be replaced with a
   new submission using fresh metadata, rather than replayed unchanged.
3. Retain the previous private key in the protected secret store for the rollback
   window. Install the same new PKCS8 private key on every Admin and restart every
   instance while credential traffic remains paused. Replacing the mounted file
   alone does not reload the key.
4. Verify instance health and query public capability on each instance directly.
   Every instance must advertise the same expected new `keyId`. Keep credential
   traffic paused if any instance still uses a different key.
5. Resume metadata and credential traffic. Clients must fetch fresh metadata and
   generate new envelopes. Retire the previous key under the secret-store retention
   policy after the rollback window; never copy either private key into logs or Git.

### Rollback order

- To revert to a legacy dashboard, first set `required=false` on every Admin, apply
  the configuration with the necessary restarts, and verify legacy compatibility
  across the pool. Then deploy the legacy dashboard and have clients reload it.
- To remove envelope support from Admin, first revert the dashboard as above and
  migrate any other envelope-only clients, or keep credential traffic paused.
  A still-open or cached envelope dashboard cannot submit credentials to a legacy
  Admin and does not downgrade to plaintext.
- To revert a key change after traffic resumed, repeat the pause and drain for the
  currently active key, restore the previous shared key on all instances, restart,
  verify matching `keyId` values, then resume traffic. Never mix keys while rolling
  back. Do not reset accounts, password hashes, token versions or business data as
  part of transport-key or application rollback.

## Validation

Run the focused protocol and MVC adapter tests, including required-mode legacy
request rejection, before publishing a paired dashboard change:

```sh
./mvnw -pl shenyu-admin -am test \
  -Dtest=PasswordEncryptionServiceTest,PasswordEncryptionAdviceTest \
  -Dsurefire.failIfNoSpecifiedTests=false
```

The MVC adapter runs before the existing DTO conversion and validation. It returns
only the original DTO fields to the unchanged controller, so immutable principal
ID authorization, password policy, stored hashes and token-version invalidation
continue to use the existing Admin implementation. No database schema migration
or replacement of existing authentication classes is required.
