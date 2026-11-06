# Admin authentication and authorization audit

This implements a bounded first delivery of #8179 on the Admin control plane.
It observes existing decisions; it adds no authorization grants, changes no HTTP
status/response bodies, and does not turn namespaces into hard tenant boundaries.

## Events and correlation

The logger `org.apache.shenyu.admin.security.audit` emits one JSON object per line.
Schema version 1 contains `timestamp_ms`, `request_id`, `actor_id`, `action`,
`namespace_id`, `resource_id`, `outcome` and a fixed `reason` category.
Missing context is `unknown`, not an identity inferred from a rejected JWT.
The actor is the authenticated Shiro principal's immutable dashboard user ID;
configured service accounts use that same ID, not a caller-supplied service name.
Resource and namespace IDs identify the attempted target; a denied target is not
proof that the object exists or is visible to the caller.

| Entry point | Observation |
| --- | --- |
| `StatelessAuthFilter` | `MISSING_TOKEN` / `REJECTED_TOKEN`; response remains 401 |
| Dashboard password login result | `LOGIN_FAILED` for missing/disabled/incorrect credentials; no asserted actor |
| MVC permission exception handler | `ACTION_DENIED`, using the mapped route template when available |
| `DataPermissionCheckerImpl` | `NAMESPACE_DENIED` or `RESOURCE_DENIED` with explicit target IDs |
| Namespace mutation membership check | `NAMESPACE_DENIED` |
| App-auth lifecycle access policy | service identity, action, namespace and owned-resource denials |
| Dashboard token revocation, password reset/change | `CREDENTIAL_REVOKED` only when the token-version update succeeds and its transaction commits |

The bearer authentication filter returns `X-Shenyu-Admin-Request-Id`. It generates
a UUID; an incoming header never supplies this ID. Later servlet-stage observations
reuse the request attribute. Existing operation events capture that same ID before
dispatch; their existing database log context is prefixed with `[request_id=...]`.
This correlates permitted configuration mutations with denied attempts without
publishing a second mutation event or changing the operation-log schema. Non-servlet
operation records keep their historical context. IDs do not establish identity or
idempotency and are not metric labels.

The first typed denial owns the request observation. A later MVC exception handler
neither logs it again nor emits its exception text/stack. Direct and wrapped Shiro
permission failures also use the bounded audit. A failure in the audit sink cannot
change a denial or fail an already committed revocation.

## Sensitive data and volume

No token or token hash, password, JWT claim, raw request URI/query, request/response
body or arbitrary exception is passed to the audit sink. Authentication exceptions
may themselves contain token-derived data, so neither the servlet authentication
filter nor JWT verification logs them. General action failures use the MVC route
**template**, not its variable values. Explicit ID/action fields are bounded to
128 UTF-16 code units and JSON-escaped, including newlines.

Each of the seven fixed failure categories has ten emissions per monotonic one-second
window, per Admin process. Excess observations increment a saturating counter; the
next admitted event reports `suppressed_since_previous`. The fixed enum-keyed state
does not grow with users, tokens, IPs or namespaces. This is sampled operational
security evidence, not a lossless forensic ledger; suppressed requests have no
individual stored record and counts are reported only when that category next emits.
Credential revocation success records bypass the failure budget, so an anonymous
failure burst cannot suppress a committed revocation. They are limited to existing
authorized mutation paths.

## Storage and access

The supplied Logback configuration writes only to
`${log.home:-./logs}/${log.app-name:-shenyu-admin}-security-audit.log`, without
propagating those records to root console/application appenders. It rotates at 10 MB,
keeps at most seven days of archives and caps archives at 100 MB; the active file is
additional. The appender is synchronous and does not create another unbounded event
queue. Operators replacing Logback must retain equivalent bounds if required.

Use a private log directory owned by the Admin service account (for example directory
mode 0700 and a restrictive process umask). Grant collection/read access only to
operators responsible for audit; apply the same controls to backups and central log
exports. The application does not encrypt these files or implement an audit viewer
ACL. Retention in an external collector is controlled separately. Existing database
operation-log access/retention remains unchanged. Verify the output directory and
collector before relying on these records; an unavailable sink is best effort.

## Coverage limits

This delivery covers the entry points listed above. It is not a universal interceptor
for every custom plugin, LDAP-provider diagnostic, bootstrap/client registration,
WebSocket machine-token handshake or app-auth credential lifecycle. Existing business
audit events remain authoritative for their mutations. Other security entry points
must explicitly adopt the typed observation API without passing credentials or body
content. Successful bearer authentication is not logged on every request.

A request ID is generated when a covered entry point needs it; whitelist/preflight
requests have no new header guarantee. Revocation observations capture actor/target
and correlation before the after-commit callback, and rollback produces no success.
No independent database/network lookup is added by the audit helper.

## Validation

Run the affected reactor, including existing response and policy tests:

```sh
./mvnw -pl shenyu-admin -am test \
  -Dtest=AdminSecurityAuditTest,AdminSecurityAuditIntegrationTest,AdminAuthenticationAuditTest,StatelessAuthFilterTest,JwtUtilsTest,DataPermissionCheckerTest,AppAuthAccessPolicyImplTest,ExceptionHandlersTest,DashboardUserServiceTest,NamespaceServiceImplTest,UserUpdatedEventTest \
  -Dsurefire.failIfNoSpecifiedTests=false -Djacoco.skip=true
./mvnw -pl shenyu-admin apache-rat:check
```

The new tests exercise raw-token/exception redaction through the real filter and JWT
helper, unchanged success/401/preflight behavior, server-generated correlation,
namespace/resource reasons, deduplication, existing operation-record correlation,
bounded concurrent bursts, JSON escaping and field lengths, sink failure, and actual
Spring/H2 transaction commit versus rollback. The clock-based limiter tests do not
sleep or depend on wall-clock timing.
