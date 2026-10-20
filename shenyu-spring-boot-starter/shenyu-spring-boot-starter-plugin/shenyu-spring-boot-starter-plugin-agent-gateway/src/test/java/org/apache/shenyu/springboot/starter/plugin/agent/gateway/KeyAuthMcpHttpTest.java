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
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Set;

import static org.apache.shenyu.springboot.starter.plugin.agent.gateway.KeyAuthMcpTestFixture.headers;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Real loopback HTTP through both public plugin entries, rule matching and tool execution.
 * The rule cache is a fixture, not an Admin/WebSocket deployment.
 */
class KeyAuthMcpHttpTest {

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void listsAndCallsOnlyEffectiveTools(final String mode) {
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode, false)) {
            JsonObject listed = fixture.success("/a", "key-a", "tools/list", "");
            assertEquals(1, listed.getAsJsonObject("result").getAsJsonArray("tools").size());
            assertEquals("read-a", listed.getAsJsonObject("result").getAsJsonArray("tools").get(0)
                    .getAsJsonObject().get("name").getAsString());
            fixture.success("/a", "key-a", "tools/call", "read-a");
            fixture.success("/a?X-Api-Key=key-a", "", "tools/call", "read-a");
            assertEquals(2, fixture.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void rejectsDenialsRuleExclusionsAndCrossTenantToolsWithoutExecution(final String mode) {
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode, false)) {
            for (String tool : List.of("read-b", "blocked", "outside", "unknown")) {
                fixture.request("/a", "key-a", "tools/call", tool).expectStatus().isForbidden();
            }
            assertEquals(0, fixture.calls());
            fixture.success("/b", "key-b", "tools/call", "read-b");
            assertEquals(1, fixture.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void invalidUnboundAndUnmatchedCredentialsDoNotGrantIdentity(final String mode) {
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode, false)) {
            fixture.request("/a", "wrong", "tools/call", "read-a").expectBody(String.class)
                    .value(body -> assertFalse(body.contains("\"isError\":false")));
            fixture.request("/a", "", "tools/call", "read-a").expectBody();
            fixture.request("/unbound", "key-unbound", "tools/call", "read-a").expectStatus().isUnauthorized();
            fixture.request("/unmatched", "key-a", "tools/call", "read-a").expectStatus().isUnauthorized();
            assertEquals(0, fixture.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void concurrentTenantsKeepIdentityAcrossSchedulers(final String mode) {
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode, false)) {
            WebClient client = WebClient.create(fixture.url());
            StepVerifier.create(Flux.range(0, 24).flatMap(index -> {
                String tenant = index % 2 == 0 ? "a" : "b";
                return client.post().uri("/" + tenant)
                        .headers(headers -> headers(headers, "key-" + tenant, "tools/call", "read-" + tenant))
                        .bodyValue(fixture.body("tools/call", "read-" + tenant)).retrieve().bodyToMono(byte[].class)
                        .doOnNext(bytes -> {
                            JsonObject result = KeyAuthMcpTestFixture.parse(new String(bytes, StandardCharsets.UTF_8))
                                    .getAsJsonObject("result").getAsJsonObject("structuredContent");
                            assertEquals("tenant-" + tenant, result.get("tenant").getAsString());
                            assertEquals("service", result.get("subject").getAsString());
                        });
            }, 8).then()).expectComplete().verify(Duration.ofSeconds(45));
            assertEquals(24, fixture.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void principalConflictIsRejectedInsteadOfOverridden(final String mode) {
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode, true)) {
            fixture.request("/a", "key-a", "tools/list", "").expectStatus().isUnauthorized();
            fixture.request("/a", "key-a", "tools/call", "read-a").expectStatus().isUnauthorized();
            assertEquals(0, fixture.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void disabledAuthenticationAndKeyRotationFailClosed(final String mode) {
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode, false)) {
            fixture.rotateKey();
            fixture.request("/a", "key-a", "tools/call", "read-a").expectBody();
            assertEquals(0, fixture.calls());
            fixture.success("/a", "rotated", "tools/call", "read-a");
            fixture.disableAuthentication();
            fixture.request("/a", "rotated", "tools/call", "read-a").expectStatus().isUnauthorized();
            assertEquals(1, fixture.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void policyIsFrozenAndRuleRevocationAppliesToNewRequests(final String mode) {
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode, false)) {
            fixture.mutateBinding();
            JsonObject result = fixture.success("/a", "key-a", "tools/call", "read-a")
                    .getAsJsonObject("result").getAsJsonObject("structuredContent");
            assertEquals("tenant-a", result.get("tenant").getAsString());
            fixture.revokeTools();
            fixture.request("/a", "key-a", "tools/call", "read-a").expectStatus().isForbidden();
            assertEquals(0, fixture.success("/a", "key-a", "tools/list", "")
                    .getAsJsonObject("result").getAsJsonArray("tools").size());
            assertEquals(1, fixture.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void cancellationIsIsolatedFromLaterRequests(final String mode) {
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode, false)) {
            fixture.pending(Sinks.one());
            HttpHeaders requestHeaders = new HttpHeaders();
            headers(requestHeaders, "key-a", "tools/call", "read-a");
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/a")
                    .headers(requestHeaders)
                    .body(fixture.body("tools/call", "read-a")));
            StepVerifier.create(fixture.execute(exchange)).then(() -> {
                assertEquals(1, fixture.calls());
                AgentMcpExecutionContext context = fixture.captured();
                assertEquals("tenant-a", context.getTenant().orElseThrow());
                assertEquals(Set.of("read-a"), context.getAllowedTools());
            }).thenCancel().verify(Duration.ofSeconds(10));
            assertTrue(fixture.cancelled());
            fixture.pending(null);
            fixture.request("/unmatched", "key-a", "tools/call", "read-a").expectStatus().isUnauthorized();
            fixture.success("/b", "key-b", "tools/call", "read-b");
            assertEquals(2, fixture.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void namespaceMustMatchTheExplicitBinding(final String mode) {
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode, false)) {
            fixture.changeNamespace("another-namespace");
            fixture.request("/a", "key-a", "tools/call", "read-a").expectStatus().isUnauthorized();
            fixture.changeNamespace(null);
            fixture.request("/a", "key-a", "tools/list", "").expectStatus().isUnauthorized();
            assertEquals(0, fixture.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void inflightExecutionRetainsItsSnapshotAfterRevocation(final String mode) {
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode, false)) {
            Sinks.One<JsonObject> pending = Sinks.one();
            fixture.pending(pending);
            HttpHeaders requestHeaders = new HttpHeaders();
            headers(requestHeaders, "key-a", "tools/call", "read-a");
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/a")
                    .headers(requestHeaders).body(fixture.body("tools/call", "read-a")));
            StepVerifier.create(fixture.execute(exchange)).then(() -> {
                final AgentMcpExecutionContext original = fixture.captured();
                fixture.revokeTools();
                fixture.mutateBinding();
                fixture.request("/a", "key-a", "tools/call", "read-a").expectStatus().isForbidden();
                assertEquals(Set.of("read-a"), original.getAllowedTools());
                assertEquals("tenant-a", original.getTenant().orElseThrow());
                assertEquals(Sinks.EmitResult.OK, pending.tryEmitValue(new JsonObject()));
            }).verifyComplete();
            assertEquals(200, exchange.getResponse().getStatusCode().value());
            assertEquals(1, fixture.calls());
        }
    }
}
