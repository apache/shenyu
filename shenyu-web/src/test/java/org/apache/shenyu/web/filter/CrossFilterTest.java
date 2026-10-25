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

package org.apache.shenyu.web.filter;

import org.apache.shenyu.common.config.ShenyuConfig.CrossFilterConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * test case for CrossFilter.
 */
public final class CrossFilterTest {

    /**
     * test method for {@link CrossFilter#filter(ServerWebExchange, WebFilterChain)}.
     */
    @Test
    public void testFilter() {
        ServerWebExchange exchangeNoHeader = MockServerWebExchange.from(MockServerHttpRequest
                .get("http://localhost:8080")
                .build());
        WebFilterChain chainNoHeader = mock(WebFilterChain.class);
        when(chainNoHeader.filter(exchangeNoHeader)).thenReturn(Mono.empty());
        CrossFilter filterNoHeader = new CrossFilter(new CrossFilterConfig());
        StepVerifier.create(filterNoHeader.filter(exchangeNoHeader, chainNoHeader))
                .expectSubscription()
                .verifyComplete();

        ServerWebExchange exchangeNormal = MockServerWebExchange.from(MockServerHttpRequest
                .get("http://localhost:8080")
                .header("Origin", "test")
                .build());
        WebFilterChain chainNormal = mock(WebFilterChain.class);
        when(chainNormal.filter(exchangeNormal)).thenReturn(Mono.empty());
        CrossFilter filterNormal = new CrossFilter(new CrossFilterConfig());
        StepVerifier.create(filterNormal.filter(exchangeNormal, chainNormal))
                .expectSubscription()
                .verifyComplete();

        ServerWebExchange exchangeOption = MockServerWebExchange.from(MockServerHttpRequest
                .options("http://localhost:8080")
                .header("Origin", "test")
                .build());
        WebFilterChain chainOption = mock(WebFilterChain.class);
        when(chainOption.filter(exchangeOption)).thenReturn(Mono.empty());
        CrossFilter filterOption = new CrossFilter(new CrossFilterConfig());
        StepVerifier.create(filterOption.filter(exchangeOption, chainOption))
                .expectSubscription()
                .verifyComplete();
    }

    /**
     * test method for {@link CrossFilter#filter(ServerWebExchange, WebFilterChain)}.
     */
    @Test
    public void testCorsWhitelist() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("http://localhost:8080")
                .header("Origin", "http://a.apache.org")
                .build());
        WebFilterChain chainNormal = mock(WebFilterChain.class);
        when(chainNormal.filter(exchange)).thenReturn(Mono.empty());
        final CrossFilterConfig filterConfig = new CrossFilterConfig();
        CrossFilterConfig.AllowedOriginConfig allowedOriginConfig = new CrossFilterConfig.AllowedOriginConfig();
        allowedOriginConfig.setDomain("apache.org");
        allowedOriginConfig.setPrefixes(new HashSet<>() {
            {
                add("a");
            }
        });
        allowedOriginConfig.setOrigins(new HashSet<>() {
            {
                add("b.apache.org");
                add("c.apache.org");
                add("http://d.apache.org");
                add("*");
            }
        });
        filterConfig.setAllowedOrigin(allowedOriginConfig);
        CrossFilter filter = new CrossFilter(filterConfig);
        StepVerifier.create(filter.filter(exchange, chainNormal))
                .expectSubscription()
                .verifyComplete();
        allowedOriginConfig.setOrigins(new HashSet<>() {
            {
                add("a.apache.org");
            }
        });
        StepVerifier.create(new CrossFilter(filterConfig).filter(exchange, chainNormal))
                .expectSubscription()
                .verifyComplete();

        filterConfig.setAllowedAnyOrigin(true);
        StepVerifier.create(new CrossFilter(filterConfig).filter(exchange, chainNormal))
                .expectSubscription()
                .verifyComplete();
        filterConfig.setAllowedOrigin(null);
        StepVerifier.create(new CrossFilter(filterConfig).filter(exchange, chainNormal))
                .expectSubscription()
                .verifyComplete();

        filterConfig.setAllowedAnyOrigin(false);
        allowedOriginConfig.setOrigins(new HashSet<>() {
            {
                add("*");
            }
        });
        allowedOriginConfig.setPrefixes(null);
        filterConfig.setAllowedOrigin(allowedOriginConfig);
        StepVerifier.create(new CrossFilter(filterConfig).filter(exchange, chainNormal))
                .expectSubscription()
                .verifyComplete();
    }

    @Test
    public void testOptionsRequestFromDisallowedOriginContinuesFilterChain() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest
                .options("http://localhost:8080")
                .header(HttpHeaders.ORIGIN, "http://disallowed.example")
                .build());
        WebFilterChain chain = mock(WebFilterChain.class);
        when(chain.filter(exchange)).thenReturn(Mono.empty());

        StepVerifier.create(new CrossFilter(new CrossFilterConfig()).filter(exchange, chain))
                .verifyComplete();

        verify(chain).filter(exchange);
        Assertions.assertNull(exchange.getResponse().getStatusCode());
    }

    @Test
    public void testOptionsRequestFromAllowedOriginIsHandled() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest
                .options("http://localhost:8080")
                .header(HttpHeaders.ORIGIN, "http://allowed.example")
                .build());
        WebFilterChain chain = mock(WebFilterChain.class);
        CrossFilterConfig filterConfig = new CrossFilterConfig();
        filterConfig.setAllowedAnyOrigin(true);

        StepVerifier.create(new CrossFilter(filterConfig).filter(exchange, chain))
                .verifyComplete();

        verify(chain, never()).filter(exchange);
        Assertions.assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
    }

    @Test
    public void testOriginRegex() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("http://localhost:8080")
                .header("Origin", "http://abc.com")
                .build());
        ServerHttpResponse exchangeResponse = exchange.getResponse();
        exchangeResponse.getHeaders().add(HttpHeaders.ACCESS_CONTROL_MAX_AGE, "0");
        exchangeResponse.getHeaders().add(HttpHeaders.ACCESS_CONTROL_ALLOW_CREDENTIALS, "*");
        WebFilterChain chainNoHeader = mock(WebFilterChain.class);
        when(chainNoHeader.filter(exchange)).thenReturn(Mono.empty());
        CrossFilterConfig crossFilterConfig = new CrossFilterConfig();
        final String regex = "^http(|s)://(.*\\.|)abc.com$";
        crossFilterConfig.getAllowedOrigin().setOriginRegex(regex);
        CrossFilter filterNoHeader = new CrossFilter(crossFilterConfig);
        StepVerifier.create(filterNoHeader.filter(exchange, chainNoHeader))
                .expectSubscription()
                .verifyComplete();
        Assertions.assertTrue(Pattern.matches(regex, "http://console.ada.abc.com"));
        Assertions.assertTrue(Pattern.matches(regex, "http://console.abc.com"));
        Assertions.assertTrue(Pattern.matches(regex, "http://abc.com"));
        Assertions.assertFalse(Pattern.matches(regex, "http://aabc.com"));
    }

    @Test
    public void testInvalidInitialOriginPatternFailsDuringConstruction() {
        CrossFilterConfig config = new CrossFilterConfig();
        config.getAllowedOrigin().setOriginRegex("[");
        Assertions.assertThrows(java.util.regex.PatternSyntaxException.class, () -> new CrossFilter(config));
    }

    @Test
    void reusesCompiledPatternAndObservesConfigurationChanges() {
        CrossFilterConfig config = new CrossFilterConfig();
        config.getAllowedOrigin().setOriginRegex("  https://allowed[.]example  ");
        CrossFilter filter = new CrossFilter(config);
        Object pattern = ReflectionTestUtils.getField(filter, "originPattern");
        Assertions.assertNotNull(pattern);
        for (int i = 0; i < 3; i++) {
            assertOrigin(filter, "https://allowed.example", true);
            assertOrigin(filter, "https://other.example", false);
            Assertions.assertSame(pattern, ReflectionTestUtils.getField(filter, "originPattern"));
        }
        config.getAllowedOrigin().setOriginRegex("https://other[.]example");
        assertOrigin(filter, "https://other.example", true);
        assertOrigin(filter, "https://allowed.example", false);
        Assertions.assertNotSame(pattern, ReflectionTestUtils.getField(filter, "originPattern"));
        config.getAllowedOrigin().setOriginRegex(" ");
        assertOrigin(filter, "https://other.example", false);
        config.setAllowedOrigin(null);
        assertOrigin(filter, "https://other.example", false);
    }

    @ParameterizedTest
    @CsvSource({
        "http://gateway.example, https://frontend.example, https://frontend.example",
        "https://gateway.example, http://frontend.example, http://frontend.example",
        "http://gateway.example, http://frontend.example, http://frontend.example",
        "https://gateway.example, frontend.example:8443, https://frontend.example:8443"
    })
    void testAllowedOriginsPreserveTheirOwnScheme(final String gateway, final String configuredOrigin, final String origin) {
        CrossFilterConfig config = new CrossFilterConfig();
        config.getAllowedOrigin().setOrigins(Set.of("  " + configuredOrigin + "  "));
        config.setAllowCredentials(true);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get(gateway).header(HttpHeaders.ORIGIN, origin));
        WebFilterChain chain = mock(WebFilterChain.class);
        when(chain.filter(exchange)).thenReturn(Mono.empty());

        filterAndComplete(new CrossFilter(config), exchange, chain);

        verify(chain).filter(exchange);
        Assertions.assertEquals(origin, exchange.getResponse().getHeaders().getAccessControlAllowOrigin());
        Assertions.assertTrue(exchange.getResponse().getHeaders().getAccessControlAllowCredentials());
        Assertions.assertTrue(exchange.getResponse().getHeaders().getVary().contains(HttpHeaders.ORIGIN));
    }

    @ParameterizedTest
    @ValueSource(strings = {"http://frontend.example", "https://frontend.example:8443", "https://other.example", "https://frontend.example.evil", "null"})
    void testAllowListDoesNotReflectDisallowedOrigins(final String origin) {
        CrossFilterConfig config = new CrossFilterConfig();
        config.getAllowedOrigin().setOrigins(Set.of("https://frontend.example"));
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.options("http://gateway.example").header(HttpHeaders.ORIGIN, origin));
        WebFilterChain chain = mock(WebFilterChain.class);
        when(chain.filter(exchange)).thenReturn(Mono.empty());

        filterAndComplete(new CrossFilter(config), exchange, chain);

        verify(chain).filter(exchange);
        Assertions.assertNull(exchange.getResponse().getHeaders().getAccessControlAllowOrigin());
        Assertions.assertNull(exchange.getResponse().getHeaders().getFirst(HttpHeaders.ACCESS_CONTROL_ALLOW_CREDENTIALS));
        Assertions.assertNull(exchange.getResponse().getStatusCode());
        Assertions.assertTrue(exchange.getResponse().getHeaders().getVary().contains(HttpHeaders.ORIGIN));
    }

    @Test
    void testAllowedCrossSchemePreflightReturnsConfiguredHeaders() {
        CrossFilterConfig config = new CrossFilterConfig();
        config.getAllowedOrigin().setOrigins(Set.of("https://frontend.example"));
        config.setAllowedMethods("GET,POST,OPTIONS");
        config.setAllowedHeaders("Content-Type,Authorization");
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.options("http://gateway.example")
                .header(HttpHeaders.ORIGIN, "https://frontend.example")
                .header(HttpHeaders.ACCESS_CONTROL_REQUEST_METHOD, "POST")
                .header(HttpHeaders.ACCESS_CONTROL_REQUEST_HEADERS, "Authorization")
                .build());
        WebFilterChain chain = mock(WebFilterChain.class);
        when(chain.filter(exchange)).thenReturn(Mono.empty());

        filterAndComplete(new CrossFilter(config), exchange, chain);

        verify(chain, never()).filter(exchange);
        HttpHeaders headers = exchange.getResponse().getHeaders();
        Assertions.assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
        Assertions.assertEquals("https://frontend.example", headers.getAccessControlAllowOrigin());
        Assertions.assertEquals(Set.of("GET", "POST", "OPTIONS"),
                Set.of(headers.getFirst(HttpHeaders.ACCESS_CONTROL_ALLOW_METHODS).split(",")));
        Assertions.assertTrue(headers.getAccessControlAllowHeaders().containsAll(Set.of("Content-Type", "Authorization")));
        Assertions.assertTrue(headers.getVary().contains(HttpHeaders.ORIGIN));
    }

    @ParameterizedTest
    @ValueSource(strings = {"Accept-Encoding", "Accept-Encoding, Origin", "Accept-Encoding, origin", "*"})
    void testVaryPreservesExistingTokensWithoutDuplicatingOrigin(final String vary) {
        CrossFilterConfig config = new CrossFilterConfig();
        config.setAllowedAnyOrigin(true);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://gateway.example")
                .header(HttpHeaders.ORIGIN, "https://frontend.example"));

        filterAndComplete(new CrossFilter(config), exchange,
                ignored -> Mono.fromRunnable(() -> exchange.getResponse().getHeaders().set(HttpHeaders.VARY, vary)));

        HttpHeaders headers = exchange.getResponse().getHeaders();
        Assertions.assertEquals("https://frontend.example", headers.getAccessControlAllowOrigin());
        Assertions.assertEquals("*".equals(vary) ? 0L : 1L, headers.getVary().stream().filter(HttpHeaders.ORIGIN::equalsIgnoreCase).count());
        Assertions.assertTrue(headers.getVary().contains("*".equals(vary) ? "*" : "Accept-Encoding"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void testWildcardModesReflectConcreteOriginsButNotNull(final boolean anyOrigin) {
        CrossFilterConfig config = new CrossFilterConfig();
        config.setAllowedAnyOrigin(anyOrigin);
        config.setAllowCredentials(true);
        config.getAllowedOrigin().setOrigins(Set.of("*"));
        CrossFilter filter = new CrossFilter(config);
        assertOrigin(filter, "https://frontend.example", true);
        assertOrigin(filter, "null", false);
    }

    @Test
    void testResponseWithoutOriginAlsoVariesByOrigin() {
        CrossFilterConfig config = new CrossFilterConfig();
        config.setAllowedAnyOrigin(true);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://gateway.example"));
        WebFilterChain chain = mock(WebFilterChain.class);
        when(chain.filter(exchange)).thenReturn(Mono.empty());

        filterAndComplete(new CrossFilter(config), exchange, chain);

        verify(chain).filter(exchange);
        Assertions.assertNull(exchange.getResponse().getHeaders().getAccessControlAllowOrigin());
        Assertions.assertNull(exchange.getResponse().getHeaders().getFirst(HttpHeaders.ACCESS_CONTROL_ALLOW_CREDENTIALS));
        Assertions.assertTrue(exchange.getResponse().getHeaders().getVary().contains(HttpHeaders.ORIGIN));
    }

    private void assertOrigin(final CrossFilter filter, final String origin, final boolean allowed) {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost").header(HttpHeaders.ORIGIN, origin));
        filterAndComplete(filter, exchange, ignored -> Mono.empty());
        Assertions.assertEquals(allowed ? origin : null, exchange.getResponse().getHeaders().getFirst(HttpHeaders.ACCESS_CONTROL_ALLOW_ORIGIN));
    }

    private void filterAndComplete(final CrossFilter filter, final ServerWebExchange exchange, final WebFilterChain chain) {
        StepVerifier.create(filter.filter(exchange, chain).then(Mono.defer(exchange.getResponse()::setComplete))).verifyComplete();
    }
}
