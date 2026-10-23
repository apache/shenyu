/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import com.google.gson.Gson;
import com.nimbusds.jose.jwk.source.ImmutableSecret;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.oauth2.jose.jws.MacAlgorithm;
import org.springframework.security.oauth2.jwt.JwtClaimsSet;
import org.springframework.security.oauth2.jwt.JwtEncoderParameters;
import org.springframework.security.oauth2.jwt.JwtValidators;
import org.springframework.security.oauth2.jwt.JwsHeader;
import org.springframework.security.oauth2.jwt.NimbusJwtEncoder;
import org.springframework.security.oauth2.jwt.NimbusReactiveJwtDecoder;
import org.springframework.security.oauth2.server.resource.introspection.NimbusReactiveOpaqueTokenIntrospector;
import org.springframework.security.web.server.WebFilterChainProxy;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Real signature/introspection authentication and public key-auth/Agent Gateway plugin chain.
 */
class PrincipalMcpHttpTest {

    private static final String ISSUER = "https://issuer.example";

    private static final byte[] KEY = "p0-02-loopback-test-key-not-a-production-secret".getBytes(StandardCharsets.UTF_8);

    @ParameterizedTest
    @CsvSource({"jwt,json", "jwt,sse", "opaque-token,json", "opaque-token,sse"})
    void authenticatedScopeControlsListAndCallWithRuleIntersection(final String source, final String mode) {
        try (Fixture fixture = new Fixture(source, mode)) {
            String token = fixture.token(Map.of("scope", "inventory:read", "tool_grants", List.of("blocked", "outside")));
            fixture.request(token, "tools/list", "").exchange().expectStatus().isOk().expectBody(String.class).value(body -> {
                assertTrue(body.contains("\"name\":\"read-a\""));
                assertTrue(!body.contains("\"name\":\"blocked\"") && !body.contains("\"name\":\"outside\""));
            });
            fixture.request(token, "tools/call", "blocked").exchange().expectStatus().isForbidden();
            fixture.request(token, "tools/call", "outside").exchange().expectStatus().isForbidden();
            assertEquals(0, fixture.gateway.calls());
            fixture.request(token, "tools/call", "read-a").exchange().expectStatus().isOk().expectBody(String.class)
                    .value(body -> assertTrue(body.contains("\"tenant\":\"tenant-a\"")));
            assertEquals(1, fixture.gateway.calls());
            fixture.gateway.revokeTools();
            fixture.request(token, "tools/list", "").exchange().expectStatus().isOk().expectBody(String.class)
                    .value(body -> assertTrue(body.contains("\"tools\":[]")));
            fixture.request(token, "tools/call", "read-a").exchange().expectStatus().isForbidden();
            assertEquals(1, fixture.gateway.calls());
        }
    }

    @ParameterizedTest
    @CsvSource({"jwt,json", "jwt,sse", "opaque-token,json", "opaque-token,sse"})
    void invalidOrConflictingIdentityNeverExecutesTool(final String source, final String mode) {
        try (Fixture fixture = new Fixture(source, mode)) {
            fixture.request("invalid-token", "tools/call", "read-a").exchange().expectStatus().isUnauthorized();
            fixture.request(fixture.token(Map.of("iss", "https://other.example")), "tools/call", "read-a").exchange().expectStatus().isUnauthorized();
            fixture.request(fixture.token(Map.of("aud", List.of("wrong"))), "tools/call", "read-a").exchange().expectStatus().isUnauthorized();
            fixture.request(fixture.token(Map.of("tenant_id", "")), "tools/call", "read-a").exchange().expectStatus().isUnauthorized();
            fixture.request(fixture.token(Map.of("scope", "inventory:read", "scp", List.of("other"))), "tools/call", "read-a")
                    .exchange().expectStatus().isUnauthorized();
            fixture.request(fixture.token(Map.of("tool_grants", List.of("*"))), "tools/call", "read-a").exchange().expectStatus().isUnauthorized();
            String token = fixture.token(Map.of("scope", "inventory:read"));
            fixture.request(token, "tools/call", "read-a", "/a", "key-a").exchange().expectStatus().isUnauthorized();
            assertEquals(0, fixture.gateway.calls());
            fixture.request(token, "tools/call", "read-a").exchange().expectStatus().isOk();
            assertEquals(1, fixture.gateway.calls());
        }
    }

    @ParameterizedTest
    @CsvSource({"jwt,json", "jwt,sse", "opaque-token,json", "opaque-token,sse"})
    void sameRpcIdConcurrentTenantsRemainIsolated(final String source, final String mode) {
        try (Fixture fixture = new Fixture(source, mode)) {
            WebClient client = WebClient.create(fixture.gateway.url());
            Flux.range(0, 16).flatMap(index -> {
                String tenant = "tenant-" + index;
                String token = fixture.token(Map.of("sub", "service-" + index, "tenant_id", tenant, "scope", "inventory:read"));
                return client.post().uri("/mcp").headers(headers -> {
                    KeyAuthMcpTestFixture.headers(headers, "", "tools/call", "read-a");
                    headers.setBearerAuth(token);
                }).bodyValue(fixture.gateway.body("tools/call", "read-a")).retrieve().bodyToMono(String.class).doOnNext(body -> {
                    assertTrue(body.contains("\"tenant\":\"" + tenant + "\""));
                    assertTrue(body.contains("\"subject\":\"service-" + index + "\""));
                });
            }, 8).collectList().block(Duration.ofSeconds(30));
            assertEquals(16, fixture.gateway.calls());
        }
    }

    @ParameterizedTest
    @CsvSource({"jwt,json", "jwt,sse", "opaque-token,json", "opaque-token,sse"})
    void credentialRevocationAndMissingScopesCannotGrantTools(final String source, final String mode) {
        try (Fixture fixture = new Fixture(source, mode)) {
            fixture.request(fixture.token(Map.of("scope", "read-a")), "tools/list", "").exchange().expectStatus().isOk().expectBody(String.class)
                    .value(body -> assertTrue(body.contains("\"tools\":[]")));
            if ("jwt".equals(source)) {
                Instant now = Instant.now();
                String expired = fixture.token(Map.of("iat", now.minusSeconds(300), "exp", now.minusSeconds(120)));
                fixture.request(expired, "tools/call", "read-a").exchange().expectStatus().isUnauthorized();
                JwtClaimsSet claims = JwtClaimsSet.builder().issuedAt(now).expiresAt(now.plusSeconds(120))
                        .issuer(ISSUER).subject("service").audience(List.of("mcp")).claim("tenant_id", "tenant-a").claim("scope", "inventory:read").build();
                String forged = new NimbusJwtEncoder(new ImmutableSecret<>("untrusted-signature-key-at-least-32-bytes".getBytes(StandardCharsets.UTF_8)))
                        .encode(JwtEncoderParameters.from(JwsHeader.with(MacAlgorithm.HS256).build(), claims)).getTokenValue();
                fixture.request(forged, "tools/call", "read-a").exchange().expectStatus().isUnauthorized();
            } else {
                String revoked = fixture.token(Map.of("scope", "inventory:read"));
                fixture.tokens.remove(revoked);
                fixture.request(revoked, "tools/call", "read-a").exchange().expectStatus().isUnauthorized();
            }
            assertEquals(0, fixture.gateway.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"jwt", "opaque-token"})
    void inFlightPrincipalSnapshotAndCancellationRemainRequestLocal(final String source) {
        PrincipalMcpSecurityProperties policy = new PrincipalMcpSecurityProperties();
        policy.setIssuer(ISSUER);
        policy.setAudience("mcp");
        org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver resolver = "jwt".equals(source)
                ? new AgentGatewayPluginConfiguration.JwtPrincipalConfiguration().agentGatewayPrincipalJwtResolver(policy)
                : new AgentGatewayPluginConfiguration.OpaquePrincipalConfiguration().agentGatewayPrincipalOpaqueResolver(policy);
        try (KeyAuthMcpTestFixture gateway = new KeyAuthMcpTestFixture("json", resolver, null)) {
            Map<String, Object> claims = Map.of("iss", ISSUER, "aud", List.of("mcp"), "sub", "service",
                    "tenant_id", "tenant-a", "tool_grants", List.of("read-a"));
            org.springframework.security.core.Authentication authentication;
            if ("jwt".equals(source)) {
                authentication = new org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken(
                        new org.springframework.security.oauth2.jwt.Jwt("test", Instant.now(), Instant.now().plusSeconds(300), Map.of("alg", "HS256"), claims), List.of());
            } else {
                authentication = new org.springframework.security.oauth2.server.resource.authentication.BearerTokenAuthentication(
                        new org.springframework.security.oauth2.core.DefaultOAuth2AuthenticatedPrincipal(claims, List.of()),
                        new org.springframework.security.oauth2.core.OAuth2AccessToken(org.springframework.security.oauth2.core.OAuth2AccessToken.TokenType.BEARER,
                                "test", Instant.now(), Instant.now().plusSeconds(300)), List.of());
            }
            var request = org.springframework.mock.http.server.reactive.MockServerHttpRequest.post("/mcp")
                    .headers(headers()).body(gateway.body("tools/call", "read-a"));
            var exchange = org.springframework.mock.web.server.MockServerWebExchange.from(request).mutate().principal(Mono.just(authentication)).build();
            gateway.pending(Sinks.one());
            StepVerifier.create(gateway.execute(exchange)).then(() -> {
                assertEquals(1, gateway.calls());
                gateway.revokeTools();
                assertEquals(Set.of("read-a"), gateway.captured().getAllowedTools());
                assertEquals("tenant-a", gateway.captured().getTenant().orElseThrow());
            }).thenCancel().verify(Duration.ofSeconds(10));
            assertTrue(gateway.cancelled());
            assertEquals(1, gateway.calls());
        }
    }

    private static org.springframework.http.HttpHeaders headers() {
        org.springframework.http.HttpHeaders headers = new org.springframework.http.HttpHeaders();
        KeyAuthMcpTestFixture.headers(headers, "", "tools/call", "read-a");
        return headers;
    }

    private static final class Fixture implements AutoCloseable {

        private final String source;

        private final Map<String, Map<String, Object>> tokens = new ConcurrentHashMap<>();

        private final LoopResources loops = LoopResources.create("principal-introspection", 2, true);

        private final DisposableServer introspection;

        private final KeyAuthMcpTestFixture gateway;

        private final WebTestClient client;

        private Fixture(final String source, final String mode) {
            this.source = source;
            introspection = HttpServer.create().host("127.0.0.1").port(0).runOn(loops).handle((request, response) -> request.receive().aggregate().asString()
                    .flatMap(body -> {
                        Map<String, Object> claims = tokens.get(body.replace("token=", ""));
                        return response.header("Content-Type", "application/json").sendString(Mono.just(new Gson().toJson(
                                Objects.isNull(claims) ? Map.of("active", false) : claims))).then();
                    })).bindNow(Duration.ofSeconds(20));
            PrincipalMcpSecurityProperties policy = new PrincipalMcpSecurityProperties();
            policy.setIssuer(ISSUER);
            policy.setAudience("mcp");
            PrincipalMcpSecurityProperties.ScopeMapping mapping = new PrincipalMcpSecurityProperties.ScopeMapping();
            mapping.setScope("inventory:read");
            mapping.setToolGrants(Set.of("read-a", "blocked"));
            mapping.setToolDenials(Set.of("blocked"));
            policy.setScopeMappings(List.of(mapping));
            ServerHttpSecurity security = ServerHttpSecurity.http().authorizeExchange(exchanges -> exchanges.anyExchange().authenticated());
            org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver resolver;
            if ("jwt".equals(source)) {
                NimbusReactiveJwtDecoder decoder = NimbusReactiveJwtDecoder.withSecretKey(new SecretKeySpec(KEY, "HmacSHA256")).macAlgorithm(MacAlgorithm.HS256).build();
                decoder.setJwtValidator(JwtValidators.createDefaultWithIssuer(ISSUER));
                security.oauth2ResourceServer(resource -> resource.jwt(jwt -> jwt.jwtDecoder(decoder)));
                resolver = new AgentGatewayPluginConfiguration.JwtPrincipalConfiguration().agentGatewayPrincipalJwtResolver(policy);
            } else {
                var inspector = new NimbusReactiveOpaqueTokenIntrospector("http://127.0.0.1:" + introspection.port() + "/introspect", "fixture-client", "fixture-secret");
                security.oauth2ResourceServer(resource -> resource.opaqueToken(opaque -> opaque.introspector(inspector)));
                resolver = new AgentGatewayPluginConfiguration.OpaquePrincipalConfiguration().agentGatewayPrincipalOpaqueResolver(policy);
            }
            gateway = new KeyAuthMcpTestFixture(mode, resolver, new WebFilterChainProxy(security.build()));
            client = WebTestClient.bindToServer().baseUrl(gateway.url()).responseTimeout(Duration.ofSeconds(30)).build();
        }

        private String token(final Map<String, Object> extra) {
            Map<String, Object> claims = new HashMap<>();
            claims.put("sub", "service");
            claims.put("tenant_id", "tenant-a");
            claims.put("iss", ISSUER);
            claims.put("aud", List.of("mcp"));
            claims.put("active", true);
            claims.putAll(extra);
            if ("jwt".equals(source)) {
                JwtClaimsSet.Builder builder = JwtClaimsSet.builder().issuedAt(Instant.now().minusSeconds(10)).expiresAt(Instant.now().plusSeconds(300));
                claims.forEach(builder::claim);
                return new NimbusJwtEncoder(new ImmutableSecret<>(KEY)).encode(
                        JwtEncoderParameters.from(JwsHeader.with(MacAlgorithm.HS256).build(), builder.build())).getTokenValue();
            }
            String token = UUID.randomUUID().toString();
            tokens.put(token, claims);
            return token;
        }

        private WebTestClient.RequestHeadersSpec<?> request(final String token, final String method, final String tool) {
            return request(token, method, tool, "/mcp", "");
        }

        private WebTestClient.RequestHeadersSpec<?> request(final String token, final String method, final String tool, final String path, final String key) {
            return client.post().uri(path).headers(headers -> {
                KeyAuthMcpTestFixture.headers(headers, key, method, tool);
                headers.setBearerAuth(token);
            }).bodyValue(gateway.body(method, tool));
        }

        @Override
        public void close() {
            gateway.close();
            introspection.disposeNow(Duration.ofSeconds(10));
            loops.disposeLater().block(Duration.ofSeconds(10));
        }
    }
}
