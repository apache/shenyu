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

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.InMemoryReactiveClientRegistrationRepository;
import org.springframework.security.oauth2.client.web.server.WebSessionServerOAuth2AuthorizedClientRepository;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.web.server.WebFilterChainProxy;
import org.springframework.security.web.server.csrf.CsrfToken;
import org.springframework.test.web.reactive.server.EntityExchangeResult;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.server.WebFilter;
import org.springframework.web.util.UriComponentsBuilder;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Application OAuth login against a loopback identity provider, including session and CSRF.
 */
class OAuthLoginMcpHttpTest {

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void loginSessionHasTrustedToolsAndRetainsCsrfProtection(final String mode) {
        try (Fixture fixture = new Fixture(mode)) {
            fixture.call("tools/call", "read-a", null).expectStatus().isForbidden();
            fixture.login();
            assertEquals(1, fixture.tokenExchanges.get());
            assertEquals(1, fixture.userInfoRequests.get());
            fixture.call("tools/call", "read-a", null).expectStatus().isForbidden();
            assertEquals(0, fixture.gateway.calls());
            String csrf = fixture.csrf();
            fixture.call("tools/list", "", csrf).expectStatus().isOk().expectBody(String.class).value(body -> {
                assertTrue(body.contains("\"name\":\"read-a\""));
                assertTrue(!body.contains("\"name\":\"blocked\""));
            });
            fixture.call("tools/call", "blocked", csrf).expectStatus().isForbidden();
            fixture.call("tools/call", "read-a", csrf).expectStatus().isOk().expectBody(String.class).value(body -> {
                assertTrue(body.contains("\"tenant\":\"tenant-login\""));
                assertTrue(body.contains("\"subject\":\"login-service\""));
            });
            assertEquals(1, fixture.gateway.calls());
            fixture.gateway.revokeTools();
            fixture.call("tools/call", "read-a", csrf).expectStatus().isForbidden();
            assertEquals(1, fixture.gateway.calls());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void invalidAuthorizationStateDoesNotCreateIdentity(final String mode) {
        try (Fixture fixture = new Fixture(mode)) {
            fixture.client.get().uri("/login/oauth2/code/trusted?code=fixture-code&state=forged")
                    .exchange().expectStatus().is3xxRedirection();
            assertEquals(0, fixture.tokenExchanges.get());
            fixture.call("tools/call", "read-a", "forged").expectStatus().isForbidden();
            assertEquals(0, fixture.gateway.calls());
        }
    }

    private static final class Fixture implements AutoCloseable {

        private final AtomicInteger tokenExchanges = new AtomicInteger();

        private final AtomicInteger userInfoRequests = new AtomicInteger();

        private final LoopResources loops = LoopResources.create("oauth-login-provider", 2, true);

        private final DisposableServer provider;

        private final KeyAuthMcpTestFixture gateway;

        private final WebTestClient client;

        private final Map<String, String> cookies = new HashMap<>();

        private Fixture(final String mode) {
            provider = HttpServer.create().host("127.0.0.1").port(0).runOn(loops).route(routes -> routes
                    .post("/token", (request, response) -> request.receive().aggregate().asString().flatMap(body -> {
                        assertTrue(body.contains("code=fixture-code"));
                        tokenExchanges.incrementAndGet();
                        return response.header("Content-Type", "application/json").sendString(Mono.just(
                                "{\"access_token\":\"fixture-access-token\",\"token_type\":\"Bearer\",\"expires_in\":300,\"scope\":\"inventory:read\"}")).then();
                    }))
                    .get("/userinfo", (request, response) -> {
                        assertEquals("Bearer fixture-access-token", request.requestHeaders().get("Authorization"));
                        userInfoRequests.incrementAndGet();
                        return response.header("Content-Type", "application/json").sendString(Mono.just(
                                "{\"sub\":\"login-service\",\"tenant_id\":\"tenant-login\",\"scope\":\"inventory:read\",\"tool_grants\":[\"blocked\"]}"));
                    })).bindNow(Duration.ofSeconds(20));
            String base = "http://127.0.0.1:" + provider.port();
            ClientRegistration registration = ClientRegistration.withRegistrationId("trusted").clientId("fixture-client").clientSecret("fixture-secret")
                    .authorizationGrantType(AuthorizationGrantType.AUTHORIZATION_CODE).redirectUri("{baseUrl}/login/oauth2/code/{registrationId}")
                    .scope("inventory:read").authorizationUri(base + "/authorize").tokenUri(base + "/token").userInfoUri(base + "/userinfo").userNameAttributeName("sub").build();
            final var security = ServerHttpSecurity.http().authorizeExchange(exchanges -> exchanges.anyExchange().authenticated())
                    .oauth2Login(login -> login.clientRegistrationRepository(new InMemoryReactiveClientRegistrationRepository(registration))
                            .authorizedClientRepository(new WebSessionServerOAuth2AuthorizedClientRepository())).build();
            PrincipalMcpSecurityProperties policy = new PrincipalMcpSecurityProperties();
            policy.setRegistrationId("trusted");
            PrincipalMcpSecurityProperties.ScopeMapping scope = new PrincipalMcpSecurityProperties.ScopeMapping();
            scope.setScope("inventory:read");
            scope.setToolGrants(Set.of("read-a"));
            scope.setToolDenials(Set.of("blocked"));
            policy.setScopeMappings(List.of(scope));
            WebFilterChainProxy proxy = new WebFilterChainProxy(security);
            WebFilter filter = (exchange, chain) -> proxy.filter(exchange, authenticated -> {
                if ("/csrf".equals(authenticated.getRequest().getPath().value())) {
                    Mono<CsrfToken> csrf = authenticated.getAttribute(CsrfToken.class.getName());
                    return csrf.flatMap(token -> {
                        authenticated.getResponse().getHeaders().set("X-Fixture-CSRF", token.getToken());
                        return authenticated.getResponse().setComplete();
                    });
                }
                return chain.filter(authenticated);
            });
            gateway = new KeyAuthMcpTestFixture(mode, new AgentGatewayPluginConfiguration.LoginPrincipalConfiguration().agentGatewayPrincipalLoginResolver(policy), filter);
            client = WebTestClient.bindToServer().baseUrl(gateway.url()).responseTimeout(Duration.ofSeconds(20)).build();
        }

        private void login() {
            EntityExchangeResult<byte[]> start = client.get().uri("/oauth2/authorization/trusted").exchange().expectStatus().is3xxRedirection().expectBody().returnResult();
            start.getResponseCookies().forEach((name, values) -> cookies.put(name, values.get(0).getValue()));
            String state = UriComponentsBuilder.fromUri(start.getResponseHeaders().getLocation()).build().getQueryParams().getFirst("state");
            assertNotNull(state);
            EntityExchangeResult<byte[]> callback = client.get().uri(java.net.URI.create(gateway.url() + "/login/oauth2/code/trusted?code=fixture-code&state=" + state))
                    .cookies(values -> cookies.forEach(values::set)).exchange().expectStatus().is3xxRedirection()
                    .expectHeader().valueEquals("Location", "/").expectBody().returnResult();
            callback.getResponseCookies().forEach((name, values) -> cookies.put(name, values.get(0).getValue()));
        }

        private String csrf() {
            EntityExchangeResult<byte[]> result = client.get().uri("/csrf").cookies(values -> cookies.forEach(values::set)).exchange().expectStatus().isOk()
                    .expectBody().returnResult();
            result.getResponseCookies().forEach((name, values) -> cookies.put(name, values.get(0).getValue()));
            String token = result.getResponseHeaders().getFirst("X-Fixture-CSRF");
            assertNotNull(token);
            return token;
        }

        private WebTestClient.ResponseSpec call(final String method, final String tool, final String csrf) {
            return client.post().uri("/mcp").cookies(values -> cookies.forEach(values::set)).headers(headers -> {
                KeyAuthMcpTestFixture.headers(headers, "", method, tool);
                if (java.util.Objects.nonNull(csrf)) {
                    headers.set("X-CSRF-TOKEN", csrf);
                }
            }).bodyValue(gateway.body(method, tool)).exchange();
        }

        @Override
        public void close() {
            gateway.close();
            provider.disposeNow(Duration.ofSeconds(10));
            loops.disposeLater().block(Duration.ofSeconds(10));
        }
    }
}
