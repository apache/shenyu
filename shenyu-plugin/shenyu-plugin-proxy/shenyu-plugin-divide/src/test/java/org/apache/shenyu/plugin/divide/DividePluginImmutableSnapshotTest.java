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

package org.apache.shenyu.plugin.divide;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Regression test pinning the contract broken in apache/shenyu#7880.
 *
 * <p>{@link UpstreamCacheManager#findUpstreamListBySelectorId} now hands out the read-only
 * snapshot published by the health-check task (a {@code List.copyOf} result) instead of a
 * fresh mutable copy. {@link DividePlugin} feeds that snapshot straight into
 * {@link CircuitBreakerCandidates}, which used to compact the list in place. This test drives
 * the real cache singleton — not a mock — with one of two upstreams tripped, so any in-place
 * mutation of the immutable snapshot surfaces as an {@link UnsupportedOperationException}.
 */
public final class DividePluginImmutableSnapshotTest {

    private static final String SELECTOR_ID = "immutable-snapshot-selector";

    private DividePlugin dividePlugin;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    private RuleData ruleData;

    private Upstream blocked;

    private Upstream healthy;

    private String blockedKey;

    private String healthyKey;

    @BeforeEach
    public void setup() {
        this.dividePlugin = new DividePlugin();
        this.chain = mock(ShenyuPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        this.ruleData = mock(RuleData.class);
        when(selectorData.getId()).thenReturn(SELECTOR_ID);
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(new DivideRuleHandle()));
        DividePluginDataHandler dividePluginDataHandler = new DividePluginDataHandler();
        dividePluginDataHandler.handlerRule(ruleData);
        dividePluginDataHandler.handlerSelector(selectorData);
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
        when(applicationContext.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        this.blocked = Upstream.builder().protocol("http://").url("127.0.0.1:9210").healthCheckEnabled(false).build();
        this.healthy = Upstream.builder().protocol("http://").url("127.0.0.1:9211").healthCheckEnabled(false).build();
        this.blockedKey = UpstreamCircuitBreaker.buildKey(SELECTOR_ID, blocked);
        this.healthyKey = UpstreamCircuitBreaker.buildKey(SELECTOR_ID, healthy);
        // publish a real immutable snapshot through the cache singleton
        UpstreamCacheManager.getInstance().submit(SELECTOR_ID, List.of(blocked, healthy));
    }

    @AfterEach
    public void tearDown() {
        UpstreamCacheManager.getInstance().removeByKey(SELECTOR_ID);
        UpstreamCircuitBreaker.getInstance().reset(blockedKey);
        UpstreamCircuitBreaker.getInstance().reset(healthyKey);
    }

    @Test
    public void trippedBreakerCompactsCandidatesWithoutMutatingTheImmutableSnapshot() {
        List<Upstream> snapshot = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(SELECTOR_ID);
        assertNotNull(snapshot);
        assertEquals(2, snapshot.size());
        // the cache hands out a read-only snapshot; mutating it must be rejected
        assertThrows(UnsupportedOperationException.class, () -> snapshot.add(blocked));

        for (int index = 0; index < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; index++) {
            UpstreamCircuitBreaker.getInstance().recordFailure(blockedKey);
        }
        assertTrue(UpstreamCircuitBreaker.getInstance().isBlocking(blockedKey));

        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test").build());
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        // before the fix this threw UnsupportedOperationException while compacting the immutable snapshot
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .verifyComplete();

        // the blocked upstream is filtered out and the request is forwarded to the healthy one
        assertEquals(healthy.buildDomain(), exchange.getAttribute(Constants.HTTP_DOMAIN));
        verify(chain, times(1)).execute(any());
        // the published snapshot stays intact and still holds both upstreams for the next request
        assertEquals(2, UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(SELECTOR_ID).size());
    }
}
