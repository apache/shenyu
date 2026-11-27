# client-registration-validation

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8501

## Requirements

### Requirement: Early validation of enabled Java registration clients

Enabled Java client starters MUST validate their bound built-in registration/protocol configuration before constructing or reusing a repository or starting registration workers. Diagnostics MUST identify the property and expected form with fixed corrective guidance without supplied values or credential-bearing causes. Only active protocol declarations and applicable built-in transport/discovery fields are validated; custom SPI settings remain delegated. Existing valid keys, aliases, property-map key casing, blank/default namespace behavior, protocol identity fallbacks and deferred automatic-port discovery MUST remain compatible. No new retry settings, public registration SPI, shared foundation constraints or wire fields are introduced. The common starter MUST supply property metadata and an inventory consistent with actual YAML/properties binding.

#### Scenario: Invalid built-in settings precede resources

- **WHEN** an enabled starter binds missing or malformed HTTP servers, an invalid required protocol field, malformed/out-of-range ports or an invalid consumed numeric delay
- **THEN** startup fails with a bounded property-specific diagnostic and neither the repository factory nor publisher is invoked

#### Scenario: Active and disabled protocols

- **WHEN** registration is disabled, a protocol starter is absent/excluded, or custom SPI properties are supplied
- **THEN** disabled registration creates no client resources; absent protocols are not validated; custom transport/address/property semantics are delegated rather than rejected by a closed built-in allowlist

#### Scenario: Legacy and automatic defaults

- **WHEN** valid existing properties, blank namespaces, supported HTTP/WebSocket automatic advertised-port inputs or server.port=0 are bound
- **THEN** existing aliases, semicolon namespace/default behavior and protocol fallback order are retained; construction does not resolve a bound server prematurely or invent an advertised address; bound-port discovery remains the responsibility of the existing resolver and sibling #8426

#### Scenario: Legacy Dubbo advertised port fallback

- **WHEN** the enabled Dubbo starter receives an omitted, blank, or literal -1 advertised port
- **THEN** validation accepts the existing fallback to the Dubbo protocol port. An explicitly configured Dubbo port must otherwise be in 1..65535. A blank required advertised port is reported as missing before resource creation.

#### Scenario: Namespace and enabled discovery validation

- **WHEN** a nonblank namespace list contains an empty or whitespace entry, or an active discovery path lacks required configuration
- **THEN** startup reports the relevant property before allocating registration resources, while disabled discovery and extension-owned settings remain untouched

#### Scenario: Guidance matches real binding

- **WHEN** a representative YAML/properties application uses documented global, protocol and transport keys
- **THEN** it binds the documented values and case-sensitive Properties keys and preserves valid defaults; metadata/hints and the property inventory describe the same consumers without advertising unsupported retry knobs

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8501#issuecomment-6909429175
