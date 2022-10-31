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

package org.apache.shenyu.plugin.resilience4j.executor;

import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.timelimiter.TimeLimiterConfig;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.plugin.resilience4j.conf.Resilience4JConf;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * CombinedExecutor test.
 */
@ExtendWith(MockitoExtension.class)
public final class CombinedExecutorTest {

    private CombinedExecutor combinedExecutor;

    @BeforeEach
    public void setUp() {
        combinedExecutor = new CombinedExecutor();
    }

    @Test
    public void normalTest() {
        Resilience4JConf conf = mock(Resilience4JConf.class);
        when(conf.getId()).thenReturn("SHENYU");
        when(conf.getRateLimiterConfig()).thenReturn(RateLimiterConfig.ofDefaults());
        when(conf.getTimeLimiterConfig()).thenReturn(TimeLimiterConfig.ofDefaults());
        when(conf.getCircuitBreakerConfig()).thenReturn(CircuitBreakerConfig.ofDefaults());
        Mono<String> mono = Mono.just("ERROR");
        StepVerifier.create(combinedExecutor.run(Mono.just("SHENYU"), throwable -> mono, conf))
                .expectSubscription()
                .expectNext("SHENYU")
                .verifyComplete();

    }

    @Test
    public void errorTest() {
        Resilience4JConf conf = mock(Resilience4JConf.class);
        when(conf.getId()).thenReturn("SHENYU");
        when(conf.getRateLimiterConfig()).thenReturn(RateLimiterConfig.ofDefaults());
        when(conf.getTimeLimiterConfig()).thenReturn(TimeLimiterConfig.ofDefaults());
        when(conf.getCircuitBreakerConfig()).thenReturn(CircuitBreakerConfig.ofDefaults());
        StepVerifier.create(combinedExecutor.run(Mono.error(new RuntimeException()), Mono::error, conf))
                .expectSubscription()
                .expectError(RuntimeException.class)
                .verify();
    }

    @Test
    public void fallbackUriTest() {
        Resilience4JConf conf = mock(Resilience4JConf.class);
        when(conf.getId()).thenReturn("SHENYU");
        when(conf.getRateLimiterConfig()).thenReturn(RateLimiterConfig.ofDefaults());
        when(conf.getTimeLimiterConfig()).thenReturn(TimeLimiterConfig.ofDefaults());
        when(conf.getCircuitBreakerConfig()).thenReturn(CircuitBreakerConfig.ofDefaults());

        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());

        StepVerifier.create(combinedExecutor.run(Mono.error(new RuntimeException()), t -> combinedExecutor.fallback(exchange, UriUtils.createUri("https://example.com"), t),
                        conf))
                .expectSubscription()
                .expectComplete()
                .verify();

        assertEquals(HttpStatus.FOUND, exchange.getResponse().getStatusCode());
        assertEquals("https://example.com", exchange.getResponse().getHeaders().getLocation().toString());
    }
}
