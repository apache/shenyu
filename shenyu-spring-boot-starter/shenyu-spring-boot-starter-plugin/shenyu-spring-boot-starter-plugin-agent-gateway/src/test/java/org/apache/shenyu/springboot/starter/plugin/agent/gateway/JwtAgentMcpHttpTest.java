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

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.nimbusds.jose.jwk.source.ImmutableSecret;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.oauth2.jose.jws.MacAlgorithm;
import org.springframework.security.oauth2.jwt.JwtClaimsSet;
import org.springframework.security.oauth2.jwt.JwtEncoderParameters;
import org.springframework.security.oauth2.jwt.JwtValidators;
import org.springframework.security.oauth2.jwt.JwsHeader;
import org.springframework.security.oauth2.jwt.NimbusJwtEncoder;
import org.springframework.security.oauth2.jwt.NimbusReactiveJwtDecoder;
import org.springframework.security.web.server.WebFilterChainProxy;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Real loopback HTTP and signature verification, not a preinstalled identity fixture.
 */
class JwtAgentMcpHttpTest {

    private static final String ISSUER = "https://issuer.example";

    private static final byte[] KEY = "jwt-mcp-test-only-key-never-use-in-production".getBytes(StandardCharsets.UTF_8);

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void missingBearerRetainsCsrfProtectionAndRequiresAuthentication(final String mode) {
        try (Fixture fixture = new Fixture(mode, "explicit-default")) {
            fixture.request(null, "tools/call").header("X-Tenant-Id", "forged").header("X-Agent-Subject", "agent")
                    .exchange().expectStatus().isForbidden();
            fixture.client.get().uri("/").exchange().expectStatus().isUnauthorized();
            assertEquals(0, fixture.calls.get());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void signedJwtControlsListingAndCallingWithDenyPrecedence(final String mode) {
        try (Fixture fixture = new Fixture(mode, null)) {
            String allowed = token(Map.of("tenant_id", "tenant-a", "tool_grants", List.of("read")), KEY, ISSUER);
            String denied = token(Map.of("tenant_id", "tenant-a", "tool_grants", List.of("read"), "tool_denies", List.of("read")), KEY, ISSUER);
            fixture.request(allowed, "tools/list").exchange().expectStatus().isOk().expectBody(String.class)
                    .value(body -> assertTrue(body.contains("\"name\":\"read\"")));
            fixture.request(denied, "tools/list").exchange().expectStatus().isOk().expectBody(String.class)
                    .value(body -> assertTrue(body.contains("\"tools\":[]")));
            fixture.request(denied, "tools/call").exchange().expectStatus().isForbidden();
            assertEquals(0, fixture.calls.get());
            fixture.request(allowed, "tools/call").exchange().expectStatus().isOk().expectBody(String.class)
                    .value(body -> assertTrue(body.contains("\"tenant\":\"tenant-a\"")));
            assertEquals(1, fixture.calls.get());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void forgedExpiredWrongIssuerAndWrongAudienceNeverReachTool(final String mode) {
        try (Fixture fixture = new Fixture(mode, "explicit-default")) {
            fixture.request(token(Map.of("tool_grants", List.of("read")), "another-untrusted-test-key-at-least-32-bytes".getBytes(StandardCharsets.UTF_8), ISSUER),
                    "tools/call").exchange().expectStatus().isUnauthorized();
            fixture.request(token(Map.of("tool_grants", List.of("read")), KEY, "https://untrusted.example"), "tools/call")
                    .exchange().expectStatus().isUnauthorized();
            fixture.request(token(Map.of("aud", List.of("other"), "tool_grants", List.of("read")), KEY, ISSUER), "tools/call")
                    .exchange().expectStatus().isUnauthorized();
            fixture.request(token(Map.of("exp", Instant.now().minusSeconds(120), "tool_grants", List.of("read")), KEY, ISSUER), "tools/call")
                    .exchange().expectStatus().isUnauthorized();
            fixture.request(token(Map.of("tenant_id", "", "tool_grants", List.of("read")), KEY, ISSUER), "tools/call")
                    .exchange().expectStatus().isUnauthorized();
            assertEquals(0, fixture.calls.get());
            fixture.request(token(Map.of("tool_grants", List.of("read")), KEY, ISSUER), "tools/call")
                    .exchange().expectStatus().isOk().expectBody(String.class)
                    .value(body -> assertTrue(body.contains("\"tenant\":\"explicit-default\"")));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void ruleIntersectionAndMissingTenantCannotBeBypassed(final String mode) {
        try (Fixture fixture = new Fixture(mode, null)) {
            fixture.request(token(Map.of("tool_grants", List.of("read")), KEY, ISSUER), "tools/call")
                    .exchange().expectStatus().isUnauthorized();
            assertEquals(0, fixture.calls.get());
        }
        try (Fixture fixture = new Fixture(mode, null, "[]")) {
            String allowed = token(Map.of("tenant_id", "tenant-a", "tool_grants", List.of("read")), KEY, ISSUER);
            fixture.request(allowed, "tools/list").exchange().expectStatus().isOk().expectBody(String.class)
                    .value(body -> assertTrue(body.contains("\"tools\":[]")));
            fixture.request(allowed, "tools/call").exchange().expectStatus().isForbidden();
            assertEquals(0, fixture.calls.get());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void concurrentSameRpcIdsKeepTenantAndSubjectBoundToTheirRequest(final String mode) {
        try (Fixture fixture = new Fixture(mode, null)) {
            WebClient client = WebClient.create(fixture.url);
            Flux.range(0, 32).flatMap(index -> {
                String tenant = "tenant-" + index;
                String jwt = token(Map.of("sub", "agent-" + index, "tenant_id", tenant, "tool_grants", List.of("read")), KEY, ISSUER);
                return client.post().uri("/").headers(headers -> {
                    headers.setBearerAuth(jwt);
                    headers.set("Content-Type", "application/json");
                    headers.set("Accept", "application/json, text/event-stream");
                    headers.set("MCP-Protocol-Version", "2026-07-28");
                    headers.set("Mcp-Method", "tools/call");
                    headers.set("Mcp-Name", "read");
                }).bodyValue(body("tools/call")).retrieve().bodyToMono(String.class)
                        .doOnNext(result -> {
                            assertTrue(result.contains("\"tenant\":\"" + tenant + "\""));
                            assertTrue(result.contains("\"subject\":\"agent-" + index + "\""));
                        });
            }, 32).collectList().block(Duration.ofSeconds(20));
            assertEquals(32, fixture.calls.get());
        }
    }

    private String token(final Map<String, Object> claims, final byte[] key, final String issuer) {
        Instant now = Instant.now();
        JwtClaimsSet.Builder builder = JwtClaimsSet.builder().issuer(issuer).subject("agent").audience(List.of("mcp"))
                .issuedAt(now.minusSeconds(180)).expiresAt(now.plusSeconds(300));
        claims.forEach(builder::claim);
        return new NimbusJwtEncoder(new ImmutableSecret<>(key)).encode(
                JwtEncoderParameters.from(JwsHeader.with(MacAlgorithm.HS256).build(), builder.build())).getTokenValue();
    }

    private static String body(final String method) {
        return "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"" + method + "\",\"params\":{"
                + ("tools/call".equals(method) ? "\"name\":\"read\"," : "")
                + "\"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}";
    }

    private static final class Fixture implements AutoCloseable {

        private final AtomicInteger calls = new AtomicInteger();

        private final LoopResources loops = LoopResources.create("jwt-mcp-http", 2, true);

        private final DisposableServer server;

        private final WebTestClient client;

        private final String url;

        Fixture(final String mode, final String defaultTenant) {
            this(mode, defaultTenant, "[\"read\"]");
        }

        Fixture(final String mode, final String defaultTenant, final String allowedTools) {
            AgentToolProvider tool = mock(AgentToolProvider.class);
            when(tool.getName()).thenReturn("read");
            when(tool.getDescription()).thenReturn("Read a fixture");
            when(tool.getInputSchema()).thenReturn(JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject());
            when(tool.getRequiredClientCapabilities()).thenReturn(new JsonObject());
            when(tool.invoke(any())).thenAnswer(ignored -> Mono.deferContextual(context -> Mono.delay(Duration.ofMillis(5)).map(tick -> {
                calls.incrementAndGet();
                JsonObject result = new JsonObject();
                result.addProperty("tenant", context.get(AgentMcpExecutionContext.class).getTenant().orElseThrow());
                result.addProperty("subject", context.get(AgentMcpExecutionContext.class).getSubject());
                return result;
            })));
            AgentMcpHttpHandler handler = new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of(tool)), "fixture", "1"),
                    new JwtAgentMcpSecurityResolver(ISSUER, "mcp", defaultTenant));
            AgentGatewayMcpConfig config = AgentGatewayMcpConfig.parse(JsonParser.parseString(
                    "{\"allowedTools\":" + allowedTools + ",\"responseMode\":\"" + mode + "\"}"), true);
            NimbusReactiveJwtDecoder decoder = NimbusReactiveJwtDecoder.withSecretKey(new SecretKeySpec(KEY, "HmacSHA256"))
                    .macAlgorithm(MacAlgorithm.HS256).build();
            decoder.setJwtValidator(JwtValidators.createDefaultWithIssuer(ISSUER));
            // Resource Server handles bearer requests; retain default CSRF protection for requests without a bearer token.
            var security = ServerHttpSecurity.http()
                    .authorizeExchange(exchanges -> exchanges.anyExchange().authenticated())
                    .oauth2ResourceServer(resource -> resource.jwt(jwt -> jwt.jwtDecoder(decoder))).build();
            var httpHandler = WebHttpHandlerBuilder.webHandler(exchange -> handler.handle(exchange, config,
                    new AgentTrafficContext("fixture-request", "mcp", "selector", "rule"), 1))
                    .filter(new WebFilterChainProxy(security)).build();
            server = HttpServer.create().host("127.0.0.1").port(0).runOn(loops).handle(new ReactorHttpHandlerAdapter(httpHandler)).bindNow();
            url = "http://127.0.0.1:" + server.port();
            client = WebTestClient.bindToServer().baseUrl(url).responseTimeout(Duration.ofSeconds(10)).build();
        }

        WebTestClient.RequestHeadersSpec<?> request(final String token, final String method) {
            return client.post().uri("/").headers(headers -> {
                if (Objects.nonNull(token)) {
                    headers.setBearerAuth(token);
                }
                headers.set("Content-Type", "application/json");
                headers.set("Accept", "application/json, text/event-stream");
                headers.set("MCP-Protocol-Version", "2026-07-28");
                headers.set("Mcp-Method", method);
                if ("tools/call".equals(method)) {
                    headers.set("Mcp-Name", "read");
                }
            }).bodyValue(body(method));
        }

        @Override
        public void close() {
            server.disposeNow();
            loops.disposeLater().block(Duration.ofSeconds(10));
        }
    }
}
