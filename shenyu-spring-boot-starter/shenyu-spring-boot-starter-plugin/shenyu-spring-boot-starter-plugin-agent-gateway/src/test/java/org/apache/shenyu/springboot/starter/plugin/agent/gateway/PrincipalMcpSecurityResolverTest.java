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
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.security.oauth2.core.DefaultOAuth2AuthenticatedPrincipal;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.user.DefaultOAuth2User;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.server.resource.authentication.BearerTokenAuthentication;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PrincipalMcpSecurityResolverTest {

    @ParameterizedTest
    @ValueSource(strings = {"jwt", "opaque-token", "oauth2-login"})
    void explicitScopeMappingIsImmutableAndDenyWins(final String source) {
        PrincipalMcpSecurityProperties policy = policy(source);
        PrincipalMcpSecurityProperties.ScopeMapping mapping = mapping("inventory:read");
        mapping.setToolGrants(Set.of("read", "denied"));
        mapping.setToolDenials(Set.of("denied"));
        policy.setScopeMappings(List.of(mapping));
        final AgentMcpSecurityResolver resolver = resolver(policy);
        mapping.setToolGrants(Set.of("admin"));
        Map<String, Object> claims = claims();
        claims.put("scope", "inventory:read admin");
        claims.put("tool_grants", List.of("direct", "denied"));
        claims.put("tool_denies", List.of("direct"));
        AgentMcpIdentity identity = resolve(resolver, authentication(source, claims));
        assertEquals(Set.of("read"), identity.getToolGrants());
        assertEquals(Set.of("direct", "denied"), identity.getToolDenials());
        assertThrows(UnsupportedOperationException.class, () -> identity.getToolGrants().clear());
    }

    @ParameterizedTest
    @ValueSource(strings = {"jwt", "opaque-token", "oauth2-login"})
    void missingScopeMappingNeverGrantsNamedTools(final String source) {
        Map<String, Object> claims = claims();
        claims.remove("tool_grants");
        claims.put("scope", "read admin");
        assertTrue(resolve(resolver(policy(source)), authentication(source, claims)).getToolGrants().isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"jwt", "opaque-token", "oauth2-login"})
    void rejectsUnverifiedWrongTypeAndMissingPrincipal(final String source) {
        AgentMcpSecurityResolver resolver = resolver(policy(source));
        Authentication authentication = authentication(source, claims());
        authentication.setAuthenticated(false);
        assertNull(resolve(resolver, authentication));
        assertNull(resolve(resolver, null));
        assertNull(resolve(resolver, authentication("jwt".equals(source) ? "opaque-token" : "jwt", claims())));
    }

    @ParameterizedTest
    @ValueSource(strings = {"jwt", "opaque-token", "oauth2-login"})
    void defaultTenantOnlyFillsAuthenticatedMissingTenant(final String source) {
        PrincipalMcpSecurityProperties properties = policy(source);
        Map<String, Object> claims = claims();
        claims.remove("tenant_id");
        assertNull(resolve(resolver(properties), authentication(source, claims)));
        properties.setDefaultTenant("explicit-default");
        AgentMcpSecurityResolver resolver = resolver(properties);
        assertEquals("explicit-default", resolve(resolver, authentication(source, claims)).getTenant().orElseThrow());
        assertNull(resolve(resolver, null));
        for (Object malformed : List.of("", " tenant", 42, "tenant" + (char) 10)) {
            claims.put("tenant_id", malformed);
            assertNull(resolve(resolver, authentication(source, claims)));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"jwt", "opaque-token", "oauth2-login"})
    void rejectsMalformedScopesAndClaims(final String source) {
        AgentMcpSecurityResolver resolver = resolver(policy(source));
        for (Object malformed : List.of(42, "read  write", "read" + (char) 9 + "write", List.of("read", 42), " read", "read\\write")) {
            Map<String, Object> claims = claims();
            claims.put("scope", malformed);
            assertNull(resolve(resolver, authentication(source, claims)));
        }
        for (Object malformed : List.of("read", List.of("*"), List.of("a".repeat(129)), List.of(42))) {
            Map<String, Object> claims = claims();
            claims.put("tool_grants", malformed);
            assertNull(resolve(resolver, authentication(source, claims)));
        }
        Map<String, Object> claims = claims();
        claims.put("scope", "read");
        claims.put("scp", List.of("write"));
        assertNull(resolve(resolver, authentication(source, claims)));
        claims.put("scp", List.of("read"));
        assertEquals(Set.of("read"), resolve(resolver, authentication(source, claims)).getToolGrants());
    }

    @ParameterizedTest
    @ValueSource(strings = {"jwt", "opaque-token"})
    void rejectsWrongIssuerAndAudience(final String source) {
        AgentMcpSecurityResolver resolver = resolver(policy(source));
        Map<String, Object> claims = claims();
        claims.put("iss", "https://untrusted.example");
        assertNull(resolve(resolver, authentication(source, claims)));
        claims = claims();
        claims.put("aud", List.of("other"));
        assertNull(resolve(resolver, authentication(source, claims)));
    }

    @Test
    void rejectsConflictingLoginRegistrationAndJwtSubjectAlias() {
        OAuth2AuthenticationToken login = (OAuth2AuthenticationToken) authentication("oauth2-login", claims());
        assertNull(resolve(resolver(policy("oauth2-login")), new OAuth2AuthenticationToken(login.getPrincipal(), login.getAuthorities(), "untrusted")));
        JwtAuthenticationToken jwt = (JwtAuthenticationToken) authentication("jwt", claims());
        assertNull(resolve(resolver(policy("jwt")), new JwtAuthenticationToken(jwt.getToken(), List.of(), "alias")));
    }

    @Test
    void rejectsDuplicateOrMalformedMappingsAndCredentialSourceConflict() {
        PrincipalMcpSecurityProperties policy = policy("jwt");
        policy.setScopeMappings(List.of(mapping("read"), mapping("read")));
        assertThrows(IllegalArgumentException.class, () -> resolver(policy));
        policy.setScopeMappings(List.of(mapping("read write")));
        assertThrows(IllegalArgumentException.class, () -> resolver(policy));
        policy.setScopeMappings(List.of());
        AgentMcpSecurityResolver resolver = new PrincipalMcpSecurityResolver<>(policy, JwtAuthenticationToken.class,
                JwtAuthenticationToken::getTokenAttributes, authentication -> true, exchange -> Mono.just(true));
        assertNull(resolve(resolver, authentication("jwt", claims())));
    }

    @ParameterizedTest
    @ValueSource(strings = {"jwt", "opaque-token", "oauth2-login"})
    void concurrentIdentitiesRemainTenantBound(final String source) {
        AgentMcpSecurityResolver resolver = resolver(policy(source));
        List<AgentMcpIdentity> identities = Flux.range(0, 32).flatMap(index -> {
            Map<String, Object> claims = claims();
            claims.put("tenant_id", "tenant-" + index);
            return resolver.resolve(exchange(authentication(source, claims))).subscribeOn(Schedulers.parallel());
        }).collectList().block();
        assertEquals(32, identities.stream().map(identity -> identity.getTenant().orElseThrow()).distinct().count());
    }

    private static PrincipalMcpSecurityProperties policy(final String source) {
        PrincipalMcpSecurityProperties properties = new PrincipalMcpSecurityProperties();
        properties.setEnabled(true);
        properties.setSource(source);
        if ("oauth2-login".equals(source)) {
            properties.setRegistrationId("trusted");
        } else {
            properties.setIssuer("https://issuer.example");
            properties.setAudience("mcp");
        }
        return properties;
    }

    private static PrincipalMcpSecurityProperties.ScopeMapping mapping(final String scope) {
        PrincipalMcpSecurityProperties.ScopeMapping mapping = new PrincipalMcpSecurityProperties.ScopeMapping();
        mapping.setScope(scope);
        mapping.setToolGrants(Set.of("read"));
        return mapping;
    }

    private static AgentMcpSecurityResolver resolver(final PrincipalMcpSecurityProperties properties) {
        return switch (properties.getSource()) {
            case "jwt" -> new AgentGatewayPluginConfiguration.JwtPrincipalConfiguration().agentGatewayPrincipalJwtResolver(properties);
            case "opaque-token" -> new AgentGatewayPluginConfiguration.OpaquePrincipalConfiguration().agentGatewayPrincipalOpaqueResolver(properties);
            case "oauth2-login" -> new AgentGatewayPluginConfiguration.LoginPrincipalConfiguration().agentGatewayPrincipalLoginResolver(properties);
            default -> throw new IllegalArgumentException("Unknown fixture source");
        };
    }

    private static Map<String, Object> claims() {
        Map<String, Object> claims = new HashMap<>();
        claims.put("sub", "service");
        claims.put("tenant_id", "tenant-a");
        claims.put("iss", "https://issuer.example");
        claims.put("aud", List.of("mcp"));
        claims.put("tool_grants", List.of("read"));
        return claims;
    }

    private static Authentication authentication(final String source, final Map<String, Object> claims) {
        List<org.springframework.security.core.GrantedAuthority> authorities = List.of(new SimpleGrantedAuthority("SCOPE_unmapped"));
        return switch (source) {
            case "jwt" -> new JwtAuthenticationToken(new Jwt("fixture-token", Instant.now(), Instant.now().plusSeconds(300), Map.of("alg", "HS256"), claims), authorities);
            case "opaque-token" -> new BearerTokenAuthentication(new DefaultOAuth2AuthenticatedPrincipal(claims, authorities),
                    new OAuth2AccessToken(OAuth2AccessToken.TokenType.BEARER, "fixture-token", Instant.now(), Instant.now().plusSeconds(300)), authorities);
            case "oauth2-login" -> new OAuth2AuthenticationToken(new DefaultOAuth2User(authorities, claims, "sub"), authorities, "trusted");
            default -> throw new IllegalArgumentException("Unknown fixture source");
        };
    }

    private static AgentMcpIdentity resolve(final AgentMcpSecurityResolver resolver, final Authentication authentication) {
        return resolver.resolve(exchange(authentication)).block();
    }

    private static org.springframework.web.server.ServerWebExchange exchange(final Authentication authentication) {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/mcp").header("X-Tenant-Id", "forged").header("tool_grants", "admin"))
                .mutate().principal(Mono.justOrEmpty(authentication)).build();
    }
}
