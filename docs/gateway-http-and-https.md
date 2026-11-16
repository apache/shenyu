# Serve HTTP alongside HTTPS

Bootstrap can expose the same gateway handler over HTTPS and an additional HTTP/1.1 port. This addresses #5981 without adding a redirect or a second plugin chain.

Enable HTTPS with Spring Boot's usual certificate configuration, then explicitly configure the additional HTTP listener:

```yaml
server:
  address: 0.0.0.0
  port: 9195
  ssl:
    enabled: true
    certificate: /etc/shenyu/tls/server.crt
    certificate-private-key: /etc/shenyu/tls/server.key

shenyu:
  netty:
    http:
      web-server-factory-enabled: true
      additional-http-port: 9196
```

Requests to `https://gateway:9195` and `http://gateway:9196` execute the same filters and gateway plugins. The HTTP listener serves plaintext directly; it does not redirect to HTTPS. Application filters can still enforce their own redirect or authentication policy.

The setting also works with the existing `shenyu.netty.http.sni` HTTPS configuration. Keep the existing SNI certificates and primary port, and add `additional-http-port` under `shenyu.netty.http`. Both listeners retain the configured bind address, event-loop resources, request decoder, socket options, compression, forwarded-header handling and Netty route providers. The extra listener uses HTTP/1.1, independently of HTTPS HTTP/2 settings.

The property is absent by default, preserving the existing single-listener behavior. It requires the ShenYu Netty server factory and an HTTPS primary listener. Valid ports are 0–65535; zero requests an ephemeral port. A nonzero additional port must differ from `server.port`. Invalid ports, a missing HTTPS configuration or a bind failure prevent startup; if the additional bind fails, the primary listener is stopped as well.

The primary HTTPS port remains the port reported by Spring Boot and used by existing registration and management integration. The extra listener does not register another gateway instance. Both listeners participate in shutdown. With `server.shutdown: graceful`, shutdown completion waits for active requests on both listeners before reporting completion.

## Verification

`ShenyuDualProtocolWebServerTest` starts real loopback servers and sends HTTP/TLS requests. It covers Boot TLS, manual SNI, shared route providers, unchanged single-listener defaults, invalid configuration, bind-failure cleanup and graceful shutdown with one active request per listener. Its certificate and key under `src/test/resources/ssl` are test fixtures only.

From the repository root, use a JDK supported by the project (17 or later):

```bash
./mvnw -B -ntp -pl shenyu-spring-boot-starter/shenyu-spring-boot-starter-gateway -am \
  -Dtest=ShenyuDualProtocolWebServerTest,ShenyuNettyWebServerConfigurationTest \
  -DfailIfNoTests=false -Djacoco.skip=true test
```

These tests verify the server factory and its lifecycle. They do not exercise a deployed Admin/gateway fleet, Kubernetes certificate rotation or external TLS termination.
