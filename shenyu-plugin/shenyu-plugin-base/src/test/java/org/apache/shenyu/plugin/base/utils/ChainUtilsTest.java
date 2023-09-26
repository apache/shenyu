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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public final class ChainUtilsTest {

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private Upstream upstream;

    @BeforeEach
    public void setUp() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http/SHENYU")
                .build());
        this.chain = mock(ShenyuPluginChain.class);

        this.upstream = Upstream.builder()
                .url("baidu.com")
                .protocol("https://")
                .build();

    }

    @Test
    public void successP2CLoadBalancerTest() {
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        Mono<Void> result = ChainUtils.executeByLoadBalancer(exchange, chain, upstream, "p2c");
        StepVerifier.create(result).expectSubscription().verifyComplete();
        assertNotEquals(0, upstream.getLag());
    }

    @Test
    public void failP2CLoadBalancerTest() {
        when(chain.execute(exchange)).thenReturn(Mono.error(Throwable::new));
        Mono<Void> result = ChainUtils.executeByLoadBalancer(exchange, chain, upstream, "p2c");
        StepVerifier.create(result).expectSubscription().verifyError();
        assertNotEquals(0, upstream.getLag());
    }

    @Test
    public void successShortestResponseLoadBalancerTest() {
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        Mono<Void> result = ChainUtils.executeByLoadBalancer(exchange, chain, upstream, "shortestResponse");
        StepVerifier.create(result).expectSubscription().verifyComplete();
        assertEquals(0, upstream.getLag());
    }

    @Test
    public void failShortestResponseLoadBalancerTest() {
        when(chain.execute(exchange)).thenReturn(Mono.error(Throwable::new));
        Mono<Void> result = ChainUtils.executeByLoadBalancer(exchange, chain, upstream, "shortestResponse");
        StepVerifier.create(result).expectSubscription().verifyError();
        assertEquals(0, upstream.getLag());
    }

    @Test
    public void successLoadBalancerTest() {
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        Mono<Void> result = ChainUtils.executeByLoadBalancer(exchange, chain, null, null);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    @Test
    public void successOthersLoadBalancerTest() {
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        Mono<Void> result = ChainUtils.executeByLoadBalancer(exchange, chain, upstream, "others");
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
}
