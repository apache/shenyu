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

package org.apache.shenyu.plugin.api.utils;

import io.netty.handler.codec.http.DefaultHttpHeaders;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.netty.http.client.HttpClientResponse;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Tests response content type precedence and result formatting.
 */
class WebFluxResultUtilsTest {

    private ApplicationContext context;

    private MockServerWebExchange exchange;

    @BeforeEach
    void setUp() {
        context = mock(ApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/").build());
    }

    @AfterEach
    void tearDown() {
        SpringBeanUtils.getInstance().setApplicationContext(null);
    }

    @ParameterizedTest
    @MethodSource("results")
    void testPreservesExplicitContentType(final Object result, final String body, final MediaType defaultType) {
        MediaType contentType = MediaType.parseMediaType("application/xml;charset=UTF-8");
        exchange.getResponse().getHeaders().setContentType(contentType);

        assertResponse(result, body, contentType);
    }

    @ParameterizedTest
    @MethodSource("results")
    void testDefaultContentType(final Object result, final String body, final MediaType defaultType) {
        assertResponse(result, body, defaultType);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testPreservesExplicitTypeOverUpstreamType(final boolean netty) {
        setUpstreamResponse(netty);
        MediaType contentType = MediaType.parseMediaType("application/problem+json;charset=UTF-8");
        exchange.getResponse().getHeaders().setContentType(contentType);

        assertResponse("{}".getBytes(StandardCharsets.UTF_8), "{}", contentType);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testFallsBackToUpstreamType(final boolean netty) {
        setUpstreamResponse(netty);

        assertResponse("<value/>".getBytes(StandardCharsets.UTF_8), "<value/>", MediaType.APPLICATION_XML);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testRetainsCustomResultFormatting(final boolean explicitType) {
        final MediaType customType = MediaType.parseMediaType("application/vnd.shenyu+json");
        ShenyuResult<Object> result = new ShenyuResult<>() {
            @Override
            public Object format(final ServerWebExchange webExchange, final Object origin) {
                return "formatted";
            }

            @Override
            public Object result(final ServerWebExchange webExchange, final Object formatted) {
                return "result:" + formatted;
            }

            @Override
            public MediaType contentType(final ServerWebExchange webExchange, final Object formatted) {
                return customType;
            }
        };
        when(context.getBean(ShenyuResult.class)).thenReturn(result);
        if (explicitType) {
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_XML);
        }

        assertResponse(Collections.singletonMap("value", true), "result:formatted", explicitType ? MediaType.APPLICATION_XML : customType);
    }

    @Test
    void testNullResultLeavesHeadersUntouched() {
        exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_XML);

        StepVerifier.create(WebFluxResultUtils.result(exchange, null)).verifyComplete();

        assertEquals(MediaType.APPLICATION_XML, exchange.getResponse().getHeaders().getContentType());
        assertEquals(-1, exchange.getResponse().getHeaders().getContentLength());
    }

    private void setUpstreamResponse(final boolean netty) {
        if (netty) {
            HttpClientResponse response = mock(HttpClientResponse.class);
            when(response.responseHeaders()).thenReturn(new DefaultHttpHeaders().set(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_XML_VALUE));
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, response);
        } else {
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR,
                    ResponseEntity.ok().contentType(MediaType.APPLICATION_XML).body(Flux.empty()));
        }
    }

    private void assertResponse(final Object result, final String body, final MediaType contentType) {
        StepVerifier.create(WebFluxResultUtils.result(exchange, result)).verifyComplete();

        assertEquals(contentType, exchange.getResponse().getHeaders().getContentType());
        assertEquals(body.getBytes(StandardCharsets.UTF_8).length, exchange.getResponse().getHeaders().getContentLength());
        StepVerifier.create(exchange.getResponse().getBodyAsString()).expectNext(body).verifyComplete();
    }

    private static Stream<Arguments> results() {
        return Stream.of(
                Arguments.of("hello", "hello", MediaType.TEXT_PLAIN),
                Arguments.of(42, "42", MediaType.TEXT_PLAIN),
                Arguments.of("你好".getBytes(StandardCharsets.UTF_8), "你好", MediaType.APPLICATION_JSON),
                Arguments.of(Collections.singletonMap("value", true), "{\"value\":true}", MediaType.APPLICATION_JSON));
    }
}
