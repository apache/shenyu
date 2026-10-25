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

package org.apache.shenyu.plugin.httpclient;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.ConnectException;
import java.net.URI;
import java.util.Objects;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The test case for {@link AbstractHttpClientPlugin}.
 */
public final class AbstractHttpClientPluginTest {

    private ShenyuPluginChain chain;

    @BeforeEach
    public void setUp() {
        chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());
    }

    @ParameterizedTest
    @ValueSource(ints = {0, -1})
    public void testZeroRetrySuccess(final int retryTimes) {
        AtomicInteger attempts = new AtomicInteger();
        TestHttpClientPlugin plugin = new TestHttpClientPlugin(exchange -> {
            attempts.incrementAndGet();
            return Mono.just("success");
        });
        ServerWebExchange exchange = createExchange(retryTimes, null, null);
        StepVerifier.create(plugin.execute(exchange, chain))
                .verifyComplete();
        assertEquals(1, attempts.get());
    }

    @Test
    public void testZeroRetryDefaultTimeoutMappedTo504() {
        AtomicInteger attempts = new AtomicInteger();
        TestHttpClientPlugin plugin = new TestHttpClientPlugin(exchange -> {
            attempts.incrementAndGet();
            return Mono.error(new TimeoutException("timeout"));
        });
        ServerWebExchange exchange = createExchange(0, null, null);
        StepVerifier.create(plugin.execute(exchange, chain))
                .expectErrorSatisfies(th -> {
                    ResponseStatusException statusException = assertInstanceOf(ResponseStatusException.class, th);
                    assertEquals(HttpStatus.GATEWAY_TIMEOUT, statusException.getStatusCode());
                })
                .verify();
        assertEquals(1, attempts.get());
    }

    @Test
    public void testZeroRetryDataBufferLimitExceptionMappedTo413() {
        AtomicInteger attempts = new AtomicInteger();
        TestHttpClientPlugin plugin = new TestHttpClientPlugin(exchange -> {
            attempts.incrementAndGet();
            return Mono.error(new DataBufferLimitException("too large"));
        });
        ServerWebExchange exchange = createExchange(0, null, null);
        StepVerifier.create(plugin.execute(exchange, chain))
                .expectErrorSatisfies(th -> {
                    ResponseStatusException statusException = assertInstanceOf(ResponseStatusException.class, th);
                    assertEquals(HttpStatus.PAYLOAD_TOO_LARGE, statusException.getStatusCode());
                })
                .verify();
        assertEquals(1, attempts.get());
    }

    @Test
    public void testZeroRetryShenyuExceptionMappedTo503() {
        AtomicInteger attempts = new AtomicInteger();
        TestHttpClientPlugin plugin = new TestHttpClientPlugin(exchange -> {
            attempts.incrementAndGet();
            return Mono.error(new ShenyuException("upstream unavailable"));
        });
        ServerWebExchange exchange = createExchange(0, null, null);
        StepVerifier.create(plugin.execute(exchange, chain))
                .expectErrorSatisfies(th -> {
                    ResponseStatusException statusException = assertInstanceOf(ResponseStatusException.class, th);
                    assertEquals(HttpStatus.SERVICE_UNAVAILABLE, statusException.getStatusCode());
                })
                .verify();
        assertEquals(1, attempts.get());
    }

    @Test
    public void testZeroRetryOtherExceptionPropagates() {
        AtomicInteger attempts = new AtomicInteger();
        ConnectException failure = new ConnectException("connection refused");
        TestHttpClientPlugin plugin = new TestHttpClientPlugin(exchange -> {
            attempts.incrementAndGet();
            return Mono.error(failure);
        });
        ServerWebExchange exchange = createExchange(0, null, null);
        StepVerifier.create(plugin.execute(exchange, chain))
                .expectErrorMatches(th -> th == failure)
                .verify();
        assertEquals(1, attempts.get());
    }

    @Test
    public void testZeroRetryFailoverStrategyTimeoutMappedTo504() {
        AtomicInteger attempts = new AtomicInteger();
        TestHttpClientPlugin plugin = new TestHttpClientPlugin(exchange -> {
            attempts.incrementAndGet();
            return Mono.error(new TimeoutException("timeout"));
        });
        ServerWebExchange exchange = createExchange(0, RetryEnum.FAILOVER.getName(), null);
        StepVerifier.create(plugin.execute(exchange, chain))
                .expectErrorSatisfies(th -> {
                    ResponseStatusException statusException = assertInstanceOf(ResponseStatusException.class, th);
                    assertEquals(HttpStatus.GATEWAY_TIMEOUT, statusException.getStatusCode());
                })
                .verify();
        assertEquals(1, attempts.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"fixed", "exponential"})
    public void testZeroRetryNonDefaultSpecTimeoutMappedTo504(final String spec) {
        AtomicInteger attempts = new AtomicInteger();
        TestHttpClientPlugin plugin = new TestHttpClientPlugin(exchange -> {
            attempts.incrementAndGet();
            return Mono.error(new TimeoutException("timeout"));
        });
        ServerWebExchange exchange = createExchange(0, null, spec);
        StepVerifier.create(plugin.execute(exchange, chain))
                .expectErrorSatisfies(th -> {
                    ResponseStatusException statusException = assertInstanceOf(ResponseStatusException.class, th);
                    assertEquals(HttpStatus.GATEWAY_TIMEOUT, statusException.getStatusCode());
                })
                .verify();
        assertEquals(1, attempts.get());
    }

    @Test
    public void testPositiveRetryExecutesStrategy() {
        AtomicInteger attempts = new AtomicInteger();
        TestHttpClientPlugin plugin = new TestHttpClientPlugin(exchange -> {
            if (attempts.incrementAndGet() == 1) {
                return Mono.error(new TimeoutException("transient timeout"));
            }
            return Mono.just("success");
        });
        ServerWebExchange exchange = createExchange(2, null, null);
        StepVerifier.create(plugin.execute(exchange, chain))
                .verifyComplete();
        assertEquals(2, attempts.get());
    }

    private ServerWebExchange createExchange(final int retryTimes, final String retryStrategy, final String retrySpec) {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test").build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://localhost:8080/test"));
        exchange.getAttributes().put(Constants.HTTP_RETRY, retryTimes);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 3000L);
        if (Objects.nonNull(retryStrategy)) {
            exchange.getAttributes().put(Constants.RETRY_STRATEGY, retryStrategy);
        }
        if (Objects.nonNull(retrySpec)) {
            exchange.getAttributes().put(Constants.HTTP_RETRY_BACK_OFF_SPEC, retrySpec);
        }
        return exchange;
    }

    private static final class TestHttpClientPlugin extends AbstractHttpClientPlugin<String> {

        private final Function<ServerWebExchange, Mono<String>> responseFunction;

        TestHttpClientPlugin(final Function<ServerWebExchange, Mono<String>> responseFunction) {
            super(Constants.BYTES_PER_MB);
            this.responseFunction = responseFunction;
        }

        @Override
        protected Mono<String> doRequest(final ServerWebExchange exchange, final String httpMethod, final URI uri, final Flux<DataBuffer> body) {
            return Mono.defer(() -> responseFunction.apply(exchange));
        }

        @Override
        public int getOrder() {
            return 0;
        }
    }
}
