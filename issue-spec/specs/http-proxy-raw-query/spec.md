# http-proxy-raw-query

## Purpose

Allow a Divide route to forward a literal query string to HTTP upstreams that compare the request target.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8652

## Requirements

### Requirement: Opt-in raw query preservation on Divide routes

The Divide rule handle MUST accept an optional boolean `preserveRawQuery`. With `true`, the HTTP upstream URI MUST preserve the current request URI's raw query, including original order, duplicate names, empty or bare parameters, and percent encoding. Omission and `false` MUST retain existing URI construction. The option MUST apply on the initial attempt and any rebuilt failover target, without changing upstream selection or retry policy. A query explicitly supplied by the resolved target takes precedence.

#### Scenario: Preserve the query on initial forwarding and failover

- **WHEN** a Divide rule enables `preserveRawQuery` and a request arrives with `tag=a%2Bb&empty=&tag=c+d&flag`
- **THEN** each HTTP attempt sends that same raw query to the selected upstream

#### Scenario: Honor explicit request and target rewrites

- **WHEN** an earlier plugin changes the request query or the resolved target supplies its own query
- **THEN** forwarding uses the current request query or, if present, the explicit target query; the chosen path is unchanged

#### Scenario: Preserve defaults and query presence

- **WHEN** the setting is absent or false
- **THEN** forwarding continues to use the existing URI construction

- **WHEN** the setting is true and the request has no query, or an explicitly empty query
- **THEN** the constructed URI preserves that distinction

## Configuration

Set `preserveRawQuery` in a Divide rule's JSON handle through the Admin API or a local rule configuration:

```json
{
  "loadBalance": "random",
  "preserveRawQuery": true
}
```

Omit the field or set it to `false` to return to the existing behavior. The rule handle uses the existing persistence and synchronization format.
