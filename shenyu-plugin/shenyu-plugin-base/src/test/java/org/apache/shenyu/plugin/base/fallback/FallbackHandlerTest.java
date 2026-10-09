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

package org.apache.shenyu.plugin.base.fallback;

import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for FallbackHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class FallbackHandlerTest {

    private ServerWebExchange exchange;

    private TestFallbackHandler testFallbackHandler;

    private TestFallbackAvoidRedirectLoopHandler testFallbackAvoidRedirectLoopHandler;

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        DispatcherHandler handler = mock(DispatcherHandler.class);
        when(context.getBean(DispatcherHandler.class)).thenReturn(handler);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/SHENYU/SHENYU")
                .remoteAddress(new InetSocketAddress(8090))
                .contextPath("/SHENYU")
                .build());
        when(handler.handle(any())).thenReturn(Mono.empty());
        this.testFallbackHandler = new TestFallbackHandler();
        this.testFallbackAvoidRedirectLoopHandler = new TestFallbackAvoidRedirectLoopHandler();
    }

    /**
     * The generate test.
     */
    @Test
    public void generateErrorTest() {
        StepVerifier.create(testFallbackHandler.withoutFallback(exchange, new RuntimeException())).expectSubscription().verifyComplete();
    }

    /**
     * The fallback test.
     */
    @Test
    public void httpFallbackPrefixTest() {
        StepVerifier.create(testFallbackHandler.fallback(exchange, null, mock(RuntimeException.class))).expectSubscription().verifyComplete();
        StepVerifier.create(testFallbackHandler.fallback(exchange, URI.create("http://127.0.0.1:8090/SHENYU"), mock(RuntimeException.class))).expectSubscription().verifyComplete();
    }

    /**
     * The fallback test.
     */
    @Test
    public void fallbackPrefixTest() {
        StepVerifier.create(testFallbackHandler.fallback(exchange, URI.create("fallback:/SHENYU"), mock(RuntimeException.class))).expectSubscription().verifyComplete();
        assertThrows(RuntimeException.class, () -> StepVerifier.create(testFallbackAvoidRedirectLoopHandler.fallback(exchange,
                URI.create("fallback:/SHENYU/SHENYU"), mock(RuntimeException.class))).expectSubscription().verifyComplete());
    }

    @ParameterizedTest
    @ValueSource(strings = {"/fallback?reason=a%26b", "fallback:/fallback?reason=a%26b"})
    void fallbackToSameEncodedQuery(final String target) {
        URI current = URI.create("http://localhost/fallback?reason=a%26b");
        MockServerWebExchange fallbackExchange = MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.GET, current));
        RuntimeException exception = new RuntimeException(new IllegalStateException("upstream failed"));
        RuntimeException thrown = assertThrows(RuntimeException.class,
                () -> testFallbackAvoidRedirectLoopHandler.fallback(fallbackExchange, URI.create(target), exception));
        assertSame(exception.getCause(), thrown.getCause());
    }

    @Test
    void relativeFallbackToDifferentQuery() {
        URI current = URI.create("http://localhost/fallback?reason=a%26b");
        URI target = URI.create("/fallback?reason=a&b");
        MockServerWebExchange fallbackExchange = MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.GET, current));

        StepVerifier.create(testFallbackAvoidRedirectLoopHandler.fallback(fallbackExchange, target, new RuntimeException("upstream failed"))).verifyComplete();

        assertEquals(HttpStatus.FOUND, fallbackExchange.getResponse().getStatusCode());
        assertEquals(target, fallbackExchange.getResponse().getHeaders().getLocation());
    }

    @Test
    void internalFallbackToDifferentQuery() {
        URI current = URI.create("http://localhost/fallback?reason=a%26b");
        MockServerWebExchange fallbackExchange = MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.GET, current));

        StepVerifier.create(testFallbackAvoidRedirectLoopHandler.fallback(fallbackExchange,
                URI.create("fallback:/fallback?reason=a&b"), new RuntimeException("upstream failed"))).verifyComplete();

        verify(SpringBeanUtils.getInstance().getBean(DispatcherHandler.class)).handle(argThat(dispatched ->
                "http://localhost/fallback?reason=a&b".equals(dispatched.getRequest().getURI().toString())));
    }

    static class TestFallbackHandler implements FallbackHandler {
        @Override
        public Mono<Void> withoutFallback(final ServerWebExchange exchange, final Throwable throwable) {
            return Mono.empty();
        }
    }

    static class TestFallbackAvoidRedirectLoopHandler implements FallbackHandler {
        @Override
        public Mono<Void> withoutFallback(final ServerWebExchange exchange, final Throwable throwable) {
            throw new RuntimeException(throwable.getCause());
        }
    }
}
