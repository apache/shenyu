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
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Tests for the divide rule-handle pool group and re-entered exchange cleanup.
 */
class DividePluginPoolGroupTest {

    private final SelectorData selector = new SelectorData();

    private final RuleData bulkRule = new RuleData();

    private final RuleData noGroupRule = new RuleData();

    private final RuleData interactiveRule = new RuleData();

    @BeforeEach
    void setUp() {
        selector.setId("pool-group-selector");
        UpstreamCacheManager.getInstance().submit(selector.getId(),
                List.of(Upstream.builder().url("127.0.0.1:18080").healthCheckEnabled(false).build()));
        bulkRule.setId("pool-group-bulk");
        bulkRule.setSelectorId(selector.getId());
        bulkRule.setHandle(handle("bulk"));
        noGroupRule.setId("pool-group-default");
        noGroupRule.setSelectorId(selector.getId());
        noGroupRule.setHandle(handle(null));
        interactiveRule.setId("pool-group-interactive");
        interactiveRule.setSelectorId(selector.getId());
        interactiveRule.setHandle(handle("interactive"));
        DividePluginDataHandler handler = new DividePluginDataHandler();
        handler.handlerRule(bulkRule);
        handler.handlerRule(noGroupRule);
        handler.handlerRule(interactiveRule);
    }

    @AfterEach
    void tearDown() {
        DividePluginDataHandler handler = new DividePluginDataHandler();
        handler.removeRule(bulkRule);
        handler.removeRule(noGroupRule);
        handler.removeRule(interactiveRule);
        UpstreamCacheManager.getInstance().removeByKey(selector.getId());
    }

    @Test
    void ruleHandlePoolGroupIsPublishedAsExchangeAttribute() {
        List<String> observed = new ArrayList<>();
        StepVerifier.create(new DividePlugin().doExecute(exchange(), current -> {
            observed.add(current.getAttribute(Constants.HTTP_POOL_GROUP));
            return Mono.empty();
        }, selector, bulkRule)).verifyComplete();

        assertEquals(List.of("bulk"), observed);
    }

    @Test
    void reenteredExchangeWithoutPoolGroupDropsPreviousSelection() {
        List<String> observed = new ArrayList<>();
        DividePlugin plugin = new DividePlugin();
        ServerWebExchange exchange = exchange();

        StepVerifier.create(plugin.doExecute(exchange, current -> {
            observed.add(current.getAttribute(Constants.HTTP_POOL_GROUP));
            return Mono.empty();
        }, selector, bulkRule)).verifyComplete();
        StepVerifier.create(plugin.doExecute(exchange, current -> {
            observed.add(current.getAttribute(Constants.HTTP_POOL_GROUP));
            return Mono.empty();
        }, selector, noGroupRule)).verifyComplete();

        assertEquals("bulk", observed.get(0));
        assertNull(observed.get(1), "a re-entered exchange must not inherit the previous pool group");
    }

    @Test
    void reenteredExchangeWithPoolGroupReplacesPreviousSelection() {
        List<String> observed = new ArrayList<>();
        DividePlugin plugin = new DividePlugin();
        ServerWebExchange exchange = exchange();

        StepVerifier.create(plugin.doExecute(exchange, current -> {
            observed.add(current.getAttribute(Constants.HTTP_POOL_GROUP));
            return Mono.empty();
        }, selector, bulkRule)).verifyComplete();
        StepVerifier.create(plugin.doExecute(exchange, current -> {
            observed.add(current.getAttribute(Constants.HTTP_POOL_GROUP));
            return Mono.empty();
        }, selector, interactiveRule)).verifyComplete();

        assertEquals(List.of("bulk", "interactive"), observed);
    }

    private ServerWebExchange exchange() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test"));
        ShenyuContext context = new ShenyuContext();
        context.setRealUrl("/test");
        exchange.getAttributes().put(Constants.CONTEXT, context);
        return exchange;
    }

    private String handle(final String poolGroup) {
        DivideRuleHandle handle = new DivideRuleHandle();
        handle.setPoolGroup(poolGroup);
        return GsonUtils.getGson().toJson(handle);
    }
}
