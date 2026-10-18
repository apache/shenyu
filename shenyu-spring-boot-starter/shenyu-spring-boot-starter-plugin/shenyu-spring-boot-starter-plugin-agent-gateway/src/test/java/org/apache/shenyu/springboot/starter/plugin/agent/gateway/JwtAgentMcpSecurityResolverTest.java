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

import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.Arguments;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.security.Principal;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class JwtAgentMcpSecurityResolverTest {

    private final JwtAgentMcpSecurityResolver resolver = new JwtAgentMcpSecurityResolver("https://issuer.example", "mcp", null);

    @Test
    void mapsVerifiedClaimsWithDenyPrecedenceAndImmutableSnapshots() {
        List<String> grants = new ArrayList<>(List.of("read", "orders.lookup"));
        Map<String, Object> claims = claims();
        claims.put("tool_grants", grants);
        claims.put("tool_denies", List.of("read"));
        AgentMcpIdentity identity = resolve(resolver, authenticated(claims));
        grants.add("admin");
        assertEquals("tenant-a", identity.getTenant().orElseThrow());
        assertEquals("agent", identity.getSubject());
        assertEquals(Set.of("orders.lookup"), identity.getToolGrants());
        assertEquals(Set.of("read"), identity.getToolDenials());
        assertThrows(UnsupportedOperationException.class, () -> identity.getToolGrants().add("admin"));
        assertThrows(UnsupportedOperationException.class, () -> identity.getToolDenials().clear());
    }

    @Test
    void unverifiedJwtAndOrdinaryPrincipalAndForgedHeadersAreNotIdentity() {
        assertNull(resolve(resolver, new JwtAuthenticationToken(jwt(claims()))));
        assertNull(resolve(resolver, (Principal) () -> "agent"));
        assertNull(resolve(resolver, null));
    }

    @Test
    void rejectsConflictingPrincipalAliasRatherThanChoosingIt() {
        assertNull(resolve(resolver, new JwtAuthenticationToken(jwt(claims()), List.of(), "other-agent")));
    }

    @Test
    void explicitDefaultTenantAppliesOnlyToAuthenticatedMissingClaim() {
        Map<String, Object> claims = claims();
        claims.remove("tenant_id");
        JwtAgentMcpSecurityResolver configured = new JwtAgentMcpSecurityResolver("https://issuer.example", "mcp", "server-tenant");
        assertNull(resolve(resolver, authenticated(claims)));
        assertEquals("server-tenant", resolve(configured, authenticated(claims)).getTenant().orElseThrow());
        assertNull(resolve(configured, null));
        assertNull(resolve(configured, new JwtAuthenticationToken(jwt(claims))));
        claims.put("tenant_id", "");
        assertNull(resolve(configured, authenticated(claims)));
    }

    @Test
    void missingGrantsNeverBecomeAllToolsOrScopes() {
        Map<String, Object> claims = claims();
        claims.remove("tool_grants");
        claims.put("scope", "read admin");
        assertTrue(resolve(resolver, authenticated(claims)).getToolGrants().isEmpty());
    }

    @ParameterizedTest
    @MethodSource("invalidClaims")
    void malformedOrWrongAuthorityClaimsAreRejected(final String key, final Object value) {
        Map<String, Object> claims = claims();
        claims.put(key, value);
        assertNull(resolve(resolver, authenticated(claims)));
    }

    private static Stream<Arguments> invalidClaims() {
        return Stream.of(
                Arguments.of("iss", "https://other.example"), Arguments.of("iss", List.of("https://issuer.example")),
                Arguments.of("aud", List.of("other")), Arguments.of("aud", "mcp"), Arguments.of("aud", List.of()), Arguments.of("aud", List.of("mcp", 42)),
                Arguments.of("tenant_id", null), Arguments.of("tenant_id", 42), Arguments.of("tenant_id", " tenant-a"),
                Arguments.of("tenant_id", "a\nb"), Arguments.of("tenant_id", "x".repeat(257)),
                Arguments.of("sub", ""), Arguments.of("sub", "agent "), Arguments.of("sub", 42),
                Arguments.of("tool_grants", "read"), Arguments.of("tool_grants", null), Arguments.of("tool_grants", List.of("*")),
                Arguments.of("tool_grants", List.of("read", 42)), Arguments.of("tool_grants", List.of("a".repeat(129))),
                Arguments.of("tool_grants", java.util.Collections.nCopies(1025, "read")),
                Arguments.of("tool_denies", "read"), Arguments.of("tool_denies", null), Arguments.of("tool_denies", List.of("read/child")));
    }

    @Test
    void rejectsInvalidDeploymentPolicyAtConstruction() {
        assertThrows(IllegalArgumentException.class, () -> new JwtAgentMcpSecurityResolver("", "mcp", null));
        assertThrows(IllegalArgumentException.class, () -> new JwtAgentMcpSecurityResolver("issuer", " ", null));
        assertThrows(IllegalArgumentException.class, () -> new JwtAgentMcpSecurityResolver("issuer", "mcp", ""));
    }

    @Test
    void concurrentSameSubjectInDifferentTenantsDoesNotShareIdentity() {
        Map<String, Object> first = claims();
        Map<String, Object> second = claims();
        second.put("tenant_id", "tenant-b");
        second.put("tool_denies", List.of("read"));
        List<AgentMcpIdentity> identities = reactor.core.publisher.Flux.merge(
                resolveAsync(resolver, authenticated(first)).subscribeOn(Schedulers.parallel()),
                resolveAsync(resolver, authenticated(second)).subscribeOn(Schedulers.parallel())).collectList().block();
        assertEquals(2, identities.size());
        assertEquals(Set.of("tenant-a", "tenant-b"), identities.stream().map(identity -> identity.getTenant().orElseThrow()).collect(java.util.stream.Collectors.toSet()));
        assertEquals(1, identities.stream().filter(identity -> identity.getToolGrants().contains("read")).count());
    }

    private AgentMcpIdentity resolve(final JwtAgentMcpSecurityResolver adapter, final Principal principal) {
        return resolveAsync(adapter, principal).block();
    }

    private Mono<AgentMcpIdentity> resolveAsync(final JwtAgentMcpSecurityResolver adapter, final Principal principal) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/agent/mcp")
                .header("Authorization", "Bearer not-a-verified-token").header("X-Tenant-Id", "forged")
                .header("X-Agent-Subject", "forged").header("tool_grants", "admin"));
        return adapter.resolve(exchange.mutate().principal(Mono.justOrEmpty(principal)).build());
    }

    private JwtAuthenticationToken authenticated(final Map<String, Object> claims) {
        return new JwtAuthenticationToken(jwt(claims), List.of());
    }

    private Jwt jwt(final Map<String, Object> claims) {
        return new Jwt("fixture-not-a-raw-credential", Instant.now(), Instant.now().plusSeconds(300), Map.of("alg", "RS256"), claims);
    }

    private Map<String, Object> claims() {
        Map<String, Object> claims = new HashMap<>();
        claims.put("iss", "https://issuer.example");
        claims.put("aud", List.of("mcp"));
        claims.put("sub", "agent");
        claims.put("tenant_id", "tenant-a");
        claims.put("tool_grants", List.of("read"));
        return claims;
    }
}
