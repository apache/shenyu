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
