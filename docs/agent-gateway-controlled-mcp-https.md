# Controlled MCP HTTPS routes

Agent Gateway remote MCP targets remain deployment-controlled. Fixed-IP HTTPS keeps
its historical behavior. A DNS hostname may be used only when deployment properties
also approve the exact hostname, port, path, and IPv4 address that the transport may
connect to.

## Deployment configuration

Set `shenyu.plugins.agent.gateway.aggregation.allowed-routes` to comma-separated
`endpoint=approvedIPv4` entries, for example:

```properties
shenyu.plugins.agent.gateway.aggregation.allowed-routes=https://mcp.example.com:443/mcp=203.0.113.10,https://192.0.2.20:8443/mcp=
```

Existing fixed-IP deployments may continue to use the historical comma-separated
`allowed-endpoints` property. At least one approved fixed endpoint or controlled
route is required when aggregation is enabled.

An empty address is valid only for a fixed-IP endpoint. A hostname without an address
is rejected. Admin plugin configuration still selects a configured endpoint and its
target-bound credential; it cannot add routes, alter approved addresses, or expand TLS
policy.

## Connection and credential semantics

- The transport uses the original HTTPS hostname for DNS-independent route identity,
  SNI, and certificate hostname validation, but connects to the approved IPv4 address.
 This prevents an implicit JVM DNS lookup from bypassing route policy.
- Each target and credential version receives its own bounded pooled client. A
 configuration refresh retires the old generation immediately for new requests, while
 an already acquired in-flight generation remains bound to its prior credential and
 client. Its client is closed after the last lease is released.
- Redirects, cookies, authentication caching, connection state, and proxy use remain
 disabled. Untrusted certificates, hostname mismatches, missing routes, and changed
 destinations fail closed.
- Route or credential replacement does not roll back a remote side effect performed by
 an in-flight request. Emergency revocation stops new requests and closes the old
 client after its leases drain.

Dynamic DNS refresh, service discovery refresh, IPv6, delegated OAuth, and enterprise
proxies remain outside this contract. Coordinate dynamic resolution with the remote
directory refresh work rather than adding a second discovery system here.
