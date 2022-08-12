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
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.HashSet;
import java.util.regex.Pattern;

import static org.mockito.Mockito.mock;
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
        allowedOriginConfig.setPrefixes(new HashSet<String>() {
            {
                add("a");
            }
        });
        allowedOriginConfig.setOrigins(new HashSet<String>() {
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
        allowedOriginConfig.setOrigins(new HashSet<String>() {
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
        allowedOriginConfig.setOrigins(new HashSet<String>() {
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
}
