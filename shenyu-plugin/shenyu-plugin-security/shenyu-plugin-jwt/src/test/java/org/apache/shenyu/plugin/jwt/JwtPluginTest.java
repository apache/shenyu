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

package org.apache.shenyu.plugin.jwt;

import com.google.common.collect.ImmutableMap;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.jwt.handle.JwtPluginDataHandler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link JwtPlugin}.
 */
public final class JwtPluginTest {

    private JwtPlugin jwtPluginUnderTest;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    private RuleData ruleData;

    private JwtPluginDataHandler jwtPluginDataHandlerUnderTest;

    @BeforeEach
    public void setUp() {
        initContext();
        selectorData = mock(SelectorData.class);
        ruleData = new RuleData();
        jwtPluginUnderTest = new JwtPlugin();
        exchange = createServerWebExchange();
        chain = mock(ShenyuPluginChain.class);
        jwtPluginDataHandlerUnderTest = new JwtPluginDataHandler();
    }

    @Test
    public void testDoExecute() {

        ruleData.setHandle("{\"converter\":[{\"jwtVal\":\"userId\",\"headerVal\":\"id\"}]}");
        jwtPluginDataHandlerUnderTest.handlerRule(ruleData);
        when(this.chain.execute(any())).thenReturn(Mono.empty());

        Mono<Void> mono = jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);

        StepVerifier.create(mono).expectSubscription().verifyComplete();
        verify(chain)
                .execute(argThat(exchange -> hasHeader(exchange, "id", "1")));
        Assertions.assertNull(exchange.getResponse().getHeaders().getFirst(HttpHeaders.WWW_AUTHENTICATE));

    }

    @Test
    public void testDoExecuteWithCustomHandleType() {

        ruleData.setHandle("{\"handleType\":\"custom\",\"customConvert\":\"customConvert\"}");
        jwtPluginDataHandlerUnderTest.handlerRule(ruleData);
        when(this.chain.execute(any())).thenReturn(Mono.empty());

        Mono<Void> mono = jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);

        StepVerifier.create(mono).expectSubscription().verifyComplete();

        verify(chain)
                .execute(argThat(exchange -> hasHeader(exchange, "custom", "customConvert")));
    }

    @Test
    public void testDoExecuteWithoutHandle() {

        when(this.chain.execute(any())).thenReturn(Mono.empty());

        Mono<Void> mono = jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);

        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    private static boolean hasHeader(final ServerWebExchange exchange, final String name, final String val) {
        return exchange.getRequest().getHeaders().get(name).contains(val);
    }

    @Test
    public void testNamed() {
        final String result = jwtPluginUnderTest.named();
        Assertions.assertEquals(PluginEnum.JWT.getName(), result);
    }

    @Test
    public void testGetOrder() {
        final int result = jwtPluginUnderTest.getOrder();
        Assertions.assertEquals(PluginEnum.JWT.getCode(), result);
    }

    @Test
    public void testBearerWithoutToken() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header("Authorization", "Bearer")
                .build());

        Mono<Void> mono = jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
        verify(chain, never()).execute(any());
    }

    @Test
    public void testAuthorizationNotBearerPrefix() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header("Authorization", "fooBearerbar")
                .build());

        Mono<Void> mono = jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
        verify(chain, never()).execute(any());
    }

    @Test
    public void testBearerWithWhitespaceOnlyToken() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header("Authorization", "Bearer  ")
                .build());

        Mono<Void> mono = jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
        verify(chain, never()).execute(any());
    }

    @Test
    public void testValidBearerAuthorization() {
        ruleData.setHandle("{\"converter\":[{\"jwtVal\":\"userId\",\"headerVal\":\"id\"}]}");
        jwtPluginDataHandlerUnderTest.handlerRule(ruleData);
        when(chain.execute(any())).thenReturn(Mono.empty());

        final String secreteKey = "shenyu-test-shenyu-test-shenyu-test";
        Map<String, Object> map = ImmutableMap.<String, Object>builder().put("userId", 1).build();
        String jwtToken = Jwts.builder()
                .claims(map)
                .issuedAt(new Date(1636371125000L))
                .expiration(new Date(new Date().getTime() + 10000L))
                .signWith(Keys.hmacShaKeyFor(secreteKey.getBytes(StandardCharsets.UTF_8)))
                .compact();

        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header("Authorization", "Bearer " + jwtToken)
                .build());

        Mono<Void> mono = jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
        verify(chain).execute(any());
        Assertions.assertNull(exchange.getResponse().getHeaders().getFirst(HttpHeaders.WWW_AUTHENTICATE));
    }

    @ParameterizedTest
    @ValueSource(strings = {"/sse", "/streamhttp", "/mcp/streamablehttp", "/mcp/message"})
    public void testMissingTokenReturnsUnauthorizedChallenge(final String path) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get(path).build());
        StepVerifier.create(jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData)).verifyComplete();
        Assertions.assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        Assertions.assertEquals("Bearer", exchange.getResponse().getHeaders().getFirst(HttpHeaders.WWW_AUTHENTICATE));
        verify(chain, never()).execute(any());
        StepVerifier.create(exchange.getResponse().getBodyAsString())
                .expectNextMatches(body -> body.contains("\"code\":401"))
                .verifyComplete();
    }

    @Test
    public void testInvalidTokenReturnsUnauthorizedChallenge() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/sse")
                .header(HttpHeaders.AUTHORIZATION, "Bearer invalid-token").build());
        StepVerifier.create(jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData)).verifyComplete();
        Assertions.assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        Assertions.assertEquals("Bearer", exchange.getResponse().getHeaders().getFirst(HttpHeaders.WWW_AUTHENTICATE));
        verify(chain, never()).execute(any());
    }

    @Test
    public void testProtectedResourceMetadataChallenge() {
        PluginData pluginData = new PluginData("pluginId", "pluginName",
                "{\"secretKey\":\"shenyu-test-shenyu-test-shenyu-test\","
                        + "\"resourceMetadataUrl\":\"https://gateway.example/.well-known/oauth-protected-resource/mcp\"}", "0", false, null);
        jwtPluginDataHandlerUnderTest.handlerPlugin(pluginData);
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/mcp/streamablehttp").build());
        StepVerifier.create(jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData)).verifyComplete();
        Assertions.assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        Assertions.assertEquals("Bearer resource_metadata=\"https://gateway.example/.well-known/oauth-protected-resource/mcp\"",
                exchange.getResponse().getHeaders().getFirst(HttpHeaders.WWW_AUTHENTICATE));
        verify(chain, never()).execute(any());
    }

    @Test
    public void testExistingAuthenticationChallengeIsPreserved() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/sse").build());
        String challenge = "Bearer realm=\"external\", resource_metadata=\"https://gateway.example/metadata\"";
        exchange.getResponse().getHeaders().set(HttpHeaders.WWW_AUTHENTICATE, challenge);
        StepVerifier.create(jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData)).verifyComplete();
        Assertions.assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        Assertions.assertEquals(challenge, exchange.getResponse().getHeaders().getFirst(HttpHeaders.WWW_AUTHENTICATE));
        verify(chain, never()).execute(any());
    }

    @Test
    public void testHttpClientReceivesAuthenticationChallenge() {
        WebTestClient.bindToWebHandler(exchange -> jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData))
                .build().get().uri("/sse").exchange()
                .expectStatus().isUnauthorized()
                .expectHeader().valueEquals(HttpHeaders.WWW_AUTHENTICATE, "Bearer")
                .expectBody(String.class).value(body -> Assertions.assertTrue(body.contains("\"code\":401")));
        verify(chain, never()).execute(any());
    }

    @Test
    public void testUnauthorizedPostDoesNotReadBody() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/mcp/streamablehttp")
                .body(Flux.error(new AssertionError("authentication must not subscribe to the request body"))));
        StepVerifier.create(jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData)).verifyComplete();
        Assertions.assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        verify(chain, never()).execute(any());
    }

    @Test
    public void testExpiredTokenReturnsUnauthorizedChallenge() {
        String expiredToken = Jwts.builder().expiration(new Date(1L))
                .signWith(Keys.hmacShaKeyFor("shenyu-test-shenyu-test-shenyu-test".getBytes(StandardCharsets.UTF_8))).compact();
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/sse")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + expiredToken).build());
        StepVerifier.create(jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData)).verifyComplete();
        Assertions.assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        Assertions.assertEquals("Bearer", exchange.getResponse().getHeaders().getFirst(HttpHeaders.WWW_AUTHENTICATE));
        verify(chain, never()).execute(any());
    }

    @Test
    public void testMissingSecretDoesNotAdvertiseAuthenticationChallenge() {
        jwtPluginDataHandlerUnderTest.handlerPlugin(new PluginData("pluginId", "pluginName", "{}", "0", false, null));
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/sse").build());
        StepVerifier.create(jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData)).verifyComplete();
        Assertions.assertNull(exchange.getResponse().getHeaders().getFirst(HttpHeaders.WWW_AUTHENTICATE));
        verify(chain, never()).execute(any());
    }

    private void initContext() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setApplicationContext(context);
        PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"secretKey\":\"shenyu-test-shenyu-test-shenyu-test\"}", "0", false, null);
        JwtPluginDataHandler jwtPluginDataHandler = new JwtPluginDataHandler();
        jwtPluginDataHandler.handlerPlugin(pluginData);
    }

    private ServerWebExchange createServerWebExchange() {

        // HMAC-SHA algorithms MUST have a size >= 256 bits
        final String secreteKey = "shenyu-test-shenyu-test-shenyu-test";

        Map<String, Object> map = ImmutableMap.<String, Object>builder().put("userId", 1).build();

        String token = Jwts.builder()
                .claims(map)
                .issuedAt(new Date(1636371125000L))
                .expiration(new Date(new Date().getTime() + 10000L))
                .signWith(Keys.hmacShaKeyFor(secreteKey.getBytes(StandardCharsets.UTF_8)))
                .compact();

        return MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header("token", token)
                .build());
    }
}
