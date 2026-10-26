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

package org.apache.shenyu.examples.plugin.agent;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import org.apache.shenyu.plugin.jwt.strategy.DefaultJwtPayloadParseStrategy;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpPrincipal;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Instant;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Date;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Real signature validation, mandatory claims and server-owned authorization.
 */
class ExampleMcpAuthenticationFilterTest {

    private static final String KEY = "example-test-key-01234567890123456789";

    private final ExampleMcpAuthenticationFilter filter = new ExampleMcpAuthenticationFilter(KEY);

    @ParameterizedTest
    @ValueSource(strings = {"missing", "malformed", "signature", "expired", "issuer", "audience", "subject", "expiration", "future", "none"})
    void shouldRejectInvalidCredentialsBeforeBusinessChain(final String kind) {
        String token = "malformed".equals(kind) ? "broken" : token(kind, "agent-a");
        MockServerHttpRequest.BodyBuilder request = MockServerHttpRequest.post("/agent/mcp");
        if (!"missing".equals(kind)) {
            request.header("Authorization", "Bearer " + token);
        }
        MockServerWebExchange exchange = MockServerWebExchange.from(request.body("{}"));
        AtomicBoolean called = new AtomicBoolean();
        StepVerifier.create(filter.filter(exchange, next -> {
            called.set(true);
            return Mono.empty();
        })).verifyComplete();
        assertFalse(called.get());
        assertEquals(401, exchange.getResponse().getStatusCode().value());
        assertTrue(exchange.getResponse().getHeaders().getFirst("WWW-Authenticate").startsWith("Bearer "));
    }

    @Test
    void shouldInstallOnlyVerifiedIdentityAndIgnoreClientPrivileges() {
        AtomicInteger principals = new AtomicInteger();
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/agent/mcp").header("Authorization", "Bearer " + token("valid", "agent-a")));
        StepVerifier.create(filter.filter(exchange, next -> next.getPrincipal().doOnNext(principal -> {
            assertTrue(principal instanceof AgentMcpPrincipal);
            assertEquals("agent-a", principal.getName());
            assertEquals(Set.of("order_status"), ((AgentMcpPrincipal) principal).getIdentity().getToolGrants());
            principals.incrementAndGet();
        }).then())).verifyComplete();
        MockServerWebExchange limited = MockServerWebExchange.from(MockServerHttpRequest.post("/agent/mcp").header("Authorization", "Bearer " + token("valid", "agent-none")));
        StepVerifier.create(filter.filter(limited, next -> next.getPrincipal().doOnNext(principal -> {
            assertEquals(Set.of(), ((AgentMcpPrincipal) principal).getIdentity().getToolGrants());
            principals.incrementAndGet();
        }).then())).verifyComplete();
        assertEquals(2, principals.get());
    }

    @Test
    void shouldRejectDuplicatedAuthorizationAndLeaveOtherPathsUntouched() {
        AtomicBoolean called = new AtomicBoolean();
        MockServerWebExchange duplicate = MockServerWebExchange.from(MockServerHttpRequest.post("/agent/mcp")
                .header("Authorization", "Bearer " + token("valid", "agent-a"), "Bearer " + token("valid", "agent-b")));
        StepVerifier.create(filter.filter(duplicate, next -> {
            called.set(true);
            return Mono.empty();
        })).verifyComplete();
        assertFalse(called.get());
        MockServerWebExchange unrelated = MockServerWebExchange.from(MockServerHttpRequest.post("/fixture/ai/chat"));
        StepVerifier.create(filter.filter(unrelated, next -> {
            called.set(true);
            return Mono.empty();
        })).verifyComplete();
        assertTrue(called.get());
    }

    private String token(final String kind, final String subject) {
        var builder = Jwts.builder().subject("subject".equals(kind) ? "unknown" : subject)
                .issuer("issuer".equals(kind) ? "wrong" : "shenyu-agent-example")
                .audience().add("audience".equals(kind) ? "wrong" : "shenyu-mcp-example").and()
                .claim("tools", Set.of("administrator", "write"));
        if (!"expiration".equals(kind)) {
            builder.expiration(Date.from(Instant.now().plusSeconds("expired".equals(kind) ? -60 : 300)));
        }
        if ("future".equals(kind)) {
            builder.notBefore(Date.from(Instant.now().plusSeconds(60)));
        }
        if ("none".equals(kind)) {
            return builder.compact();
        }
        String key = "signature".equals(kind) ? "different-test-key-012345678901234567" : KEY;
        return builder.signWith(Keys.hmacShaKeyFor(key.getBytes(java.nio.charset.StandardCharsets.UTF_8))).compact();
    }

    @ParameterizedTest
    @ValueSource(strings = {"\"shenyu-mcp-example\"", "[\"shenyu-mcp-example\"]"})
    void shouldAcceptExternallyEncodedAudienceForms(final String audience) throws Exception {
        Base64.Encoder encoder = Base64.getUrlEncoder().withoutPadding();
        String payload = "{\"sub\":\"agent-a\",\"iss\":\"shenyu-agent-example\",\"aud\":" + audience
                + ",\"exp\":" + Instant.now().plusSeconds(300).getEpochSecond() + "}";
        String unsigned = encoder.encodeToString("{\"alg\":\"HS256\"}".getBytes(StandardCharsets.UTF_8))
                + "." + encoder.encodeToString(payload.getBytes(StandardCharsets.UTF_8));
        Mac mac = Mac.getInstance("HmacSHA256");
        mac.init(new SecretKeySpec(KEY.getBytes(StandardCharsets.UTF_8), "HmacSHA256"));
        String external = unsigned + "." + encoder.encodeToString(mac.doFinal(unsigned.getBytes(StandardCharsets.US_ASCII)));
        var verified = new DefaultJwtPayloadParseStrategy().parse(KEY, external);
        assertNotNull(verified);
        AtomicBoolean called = new AtomicBoolean();
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/agent/mcp")
                .header("Authorization", "Bearer " + external));
        StepVerifier.create(filter.filter(exchange, next -> {
            called.set(true);
            return next.getPrincipal().doOnNext(principal -> assertEquals("agent-a", principal.getName())).then();
        })).verifyComplete();
        assertTrue(called.get(), "Verified claim value types: " + verified.entrySet().stream()
                .map(entry -> entry.getKey() + ":" + entry.getValue().getClass().getSimpleName()).toList());
    }
}
