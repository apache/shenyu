/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License,  Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,  software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,  either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.base.fallback;

import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.net.URI;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for FallbackHandler.
 */
@RunWith(MockitoJUnitRunner.class)
public final class FallbackHandlerTest {

    private ServerWebExchange exchange;

    private TestFallbackHandler testFallbackHandler;

    @Before
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
    }

    /**
     * The generate test.
     */
    @Test
    public void generateErrorTest() {
        StepVerifier.create(testFallbackHandler.generateError(exchange, new RuntimeException())).expectSubscription().verifyComplete();
    }

    /**
     * The fallback test.
     */
    @Test
    public void fallbackTest() {
        StepVerifier.create(testFallbackHandler.fallback(exchange, null, mock(RuntimeException.class))).expectSubscription().verifyComplete();
        StepVerifier.create(testFallbackHandler.fallback(exchange, URI.create("http://127.0.0.1:8090/SHENYU"), mock(RuntimeException.class))).expectSubscription().verifyComplete();
    }

    static class TestFallbackHandler implements FallbackHandler {
        @Override
        public Mono<Void> generateError(final ServerWebExchange exchange, final Throwable throwable) {
            return Mono.empty();
        }
    }
}
