# Apache ShenYu Security Model

This document defines the security model and trust boundaries of Apache ShenYu. It is intended for:

- **Operators**: understand required deployment controls and security assumptions.
- **Security researchers**: determine whether a reported behavior falls within ShenYu's intended trust boundaries.
- **ASF Security team**: triage incoming vulnerability reports against this model.

Reports about behavior that violates this model are in scope. Reports about behavior that assumes a boundary explicitly excluded by this model are out of scope.

---

## 1. Deployment Boundary

**The Admin service (shenyu-admin) is designed to operate within a trusted internal network.**

The Admin port (default: `9095`) must **not** be exposed to the public internet. The Admin service binds to `0.0.0.0` by default for convenience in containerized and internal-network deployments; this is not an indication that it is safe for external exposure.

Operators are responsible for:

- Placing the Admin service behind a firewall or private VPC.
- Using network-level access controls (security groups, ingress rules, VPN) to restrict access to the Admin port.
- Optionally binding the Admin server to a specific internal address in production.

Exposing the Admin port to the public internet is a deployment misconfiguration, not a vulnerability in ShenYu.

## 2. Authentication Boundary

**All authenticated Admin users are fully trusted. Account provisioning is the security boundary.**

The Admin service uses Apache Shiro for authentication. Once a user is authenticated (via username/password, LDAP, or other configured mechanisms), they are considered a trusted operator of the ShenYu deployment.

The security boundary is:

- **Account creation and credential management** — only authorized personnel should be able to provision Admin accounts.
- **Credential storage** — passwords and secrets stored by ShenYu (e.g., in the database) must be protected.

Actions performed by an authenticated Admin user (including any user with a valid login) are not security vulnerabilities. If an attacker can authenticate, the deployment is already compromised.

## 3. RBAC Model

**`@RequiresPermissions` controls UI feature visibility, not security isolation between authenticated Admin users.**

ShenYu implements a role-based access control (RBAC) system using Apache Shiro annotations (`@RequiresPermissions`). This system is designed to:

- Simplify the Admin UI by showing each user only the features relevant to their role.
- Prevent accidental misconfiguration by limiting which users can modify specific resources (plugins, rules, selectors, etc.).

RBAC in ShenYu is **not** a hard security boundary. It does **not** protect against:

- A malicious authenticated user intentionally escalating privileges.
- A user discovering or invoking API endpoints they are not assigned to through means other than the Admin UI.
- Cross-tenant isolation between Admin users of the same deployment.

**All authenticated Admin users share the same trust domain.** If you require strict isolation between operators, deploy separate ShenYu Admin instances.

## 4. WebSocket Sync Channel

**The `/websocket` endpoint is intentionally unauthenticated and must be protected by network-level access controls.**

The Admin service exposes a WebSocket endpoint at `/websocket` for Gateway (bootstrap) instances to receive real-time configuration updates. This endpoint is explicitly excluded from Shiro authentication (see `shenyu.shiro.white-list` in `application.yml`).

Because the Gateway must connect to this endpoint to synchronize plugin, selector, and rule data, the WebSocket channel operates without authentication at the application layer. The security of this channel depends entirely on:

- **Network isolation**: the `/websocket` endpoint must only be reachable by Gateway instances within the trusted internal network.
- **Firewall rules**: block external access to the Admin port, which also protects the WebSocket endpoint.

Anyone who can reach the `/websocket` endpoint can receive the full configuration state of the Gateway, including plugin rules, upstream service addresses, and authentication keys. This is by design: within the trusted network, the Gateway is a legitimate consumer of this data.

## 5. Control Plane vs. Data Plane

**Admin (control plane) and Gateway (data plane) are separate trust domains.**

| Component | Role | Trust Level |
|---|---|---|
| shenyu-admin | Control plane — manages configuration, plugins, rules, metadata | Trusted internal only |
| shenyu-bootstrap | Data plane — handles live traffic routing, rate limiting, auth | Exposed to traffic (internet-facing or DMZ) |

Key implications:

- The Gateway does **not** trust the Admin implicitly for runtime traffic decisions. The Gateway enforces its own authentication, rate limiting, and routing rules against incoming requests.
- Compromise of the Admin does **not** directly compromise traffic flowing through the Gateway, but it does allow an attacker to modify Gateway configuration (add/remove plugins, modify routing rules, etc.).
- Compromise of the Gateway does **not** grant access to the Admin or its database.
- The WebSocket sync channel (`/websocket`) is the bridge between these trust domains and must be protected as described in Section 4.

## 6. Database Security

**The database is trusted infrastructure. Database compromise is an independent security event outside ShenYu's software threat model.**

ShenYu stores configuration, user credentials, plugin data, and metadata in a database (H2 for development, MySQL/PostgreSQL for production).

ShenYu assumes:

- The database is deployed in a secured environment with access controls independent of ShenYu.
- Database credentials used by ShenYu are protected and not exposed.
- The database network port is not exposed to untrusted networks.

If an attacker gains direct access to the database (e.g., via SQL injection in a separate application, stolen credentials, or network misconfiguration), they can read or modify all ShenYu state. This is an infrastructure-level compromise, not a vulnerability in ShenYu's application logic.

## 7. Scope Summary

**In scope** (report to [security@apache.org](mailto:security@apache.org)):

- Unauthenticated remote code execution.
- Authentication bypass allowing access without valid credentials.
- Gateway (bootstrap) request-handling vulnerabilities exploitable through crafted HTTP traffic.
- Plugin-level vulnerabilities that allow bypassing Gateway-enforced authentication or rate limiting.
- Injection attacks (SQL, command, template) reachable through unauthenticated Gateway request paths.

**Out of scope** (deployment or infrastructure concerns):

- Actions performed by an authenticated Admin user.
- Misuse of features by an authenticated Admin user (e.g., an Admin user modifying rules they are not "supposed" to access via RBAC).
- Exposure of the Admin port or WebSocket endpoint to untrusted networks.
- Direct database access by an attacker with database credentials.
- Vulnerabilities in third-party infrastructure (LDAP servers, databases, Kubernetes clusters) used alongside ShenYu.

---

## References

- [Apache ShenYu Website](https://shenyu.apache.org/)
- [Security Vulnerability Reporting](https://www.apache.org/security/)
- [Apache Project Maturity Model](https://community.apache.org/apache-way/apache-project-maturity-model.html)
