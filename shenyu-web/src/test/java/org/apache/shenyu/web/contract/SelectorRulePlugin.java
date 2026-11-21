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

package org.apache.shenyu.web.contract;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

/**
 * A minimal {@link AbstractShenyuPlugin} fixture that reaches the downstream chain after a
 * selector and rule match.
 *
 * <p>The fixture records the matched selector and rule, so a test can distinguish a matched
 * request from a miss without inspecting production caches.</p>
 */
public final class SelectorRulePlugin extends AbstractShenyuPlugin {

    private final String name;

    private final int order;

    private final AtomicInteger doExecuteCount = new AtomicInteger();

    private final AtomicReference<SelectorData> matchedSelector = new AtomicReference<>();

    private final AtomicReference<RuleData> matchedRule = new AtomicReference<>();

    /**
     * Creates the fixture.
     *
     * @param name plugin name
     * @param order plugin order
     */
    public SelectorRulePlugin(final String name, final int order) {
        this.name = name;
        this.order = order;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        doExecuteCount.incrementAndGet();
        matchedSelector.set(selector);
        matchedRule.set(rule);
        return chain.execute(exchange);
    }

    @Override
    public int getOrder() {
        return order;
    }

    @Override
    public String named() {
        return name;
    }

    /**
     * Returns how many times selector/rule matching reached the plugin body.
     *
     * @return body execution count
     */
    public int getDoExecuteCount() {
        return doExecuteCount.get();
    }

    /**
     * Returns the last matched selector.
     *
     * @return selector or null before a match
     */
    public SelectorData getMatchedSelector() {
        return matchedSelector.get();
    }

    /**
     * Returns the last matched rule.
     *
     * @return rule or null before a match
     */
    public RuleData getMatchedRule() {
        return matchedRule.get();
    }

    /**
     * Clears the recorded counters and match data.
     */
    public void reset() {
        doExecuteCount.set(0);
        matchedSelector.set(null);
        matchedRule.set(null);
    }
}
