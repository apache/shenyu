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

package org.apache.shenyu.plugin.sentinel.fallback;

import com.alibaba.csp.sentinel.slots.block.authority.AuthorityException;
import com.alibaba.csp.sentinel.slots.block.degrade.DegradeException;
import com.alibaba.csp.sentinel.slots.block.flow.FlowException;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class SentinelFallbackHandlerTest {

    private SentinelFallbackHandler fallbackHandler;

    private ServerWebExchange exchange;

    @BeforeEach
    public void setUp() {
        fallbackHandler = new SentinelFallbackHandler();
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/Sentinel/Sentinel")
                .remoteAddress(new InetSocketAddress(8090))
                .contextPath("/Sentinel")
                .build());
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
    }

    /**
     * test DegradeException.
     */
    @Test
    public void testDegradeException() {
        StepVerifier.create(fallbackHandler.withoutFallback(exchange, new DegradeException("Sentinel"))).expectSubscription().verifyComplete();
    }

    /**
     * test FlowException.
     */
    @Test
    public void testFlowException() {
        StepVerifier.create(fallbackHandler.withoutFallback(exchange, new FlowException(""))).expectSubscription().verifyComplete();
    }

    /**
     * test BlockException.
     */
    @Test
    public void testBlockException() {
        StepVerifier.create(fallbackHandler.withoutFallback(exchange, new AuthorityException("Sentinel"))).expectSubscription().verifyComplete();
    }

    /**
     * test RuntimeException.
     */
    @Test
    public void testRuntimeException() {
        StepVerifier.create(fallbackHandler.withoutFallback(exchange, new RuntimeException())).expectSubscription().verifyError();
    }

    @Test
    public void testFallbackUri() {
        StepVerifier.create(fallbackHandler.fallback(exchange, UriUtils.createUri("https://example.com"), new RuntimeException())).expectSubscription().verifyComplete();
        assertEquals(HttpStatus.FOUND, exchange.getResponse().getStatusCode());
        assertEquals("https://example.com", Objects.requireNonNull(exchange.getResponse().getHeaders().getLocation()).toString());
    }
}
