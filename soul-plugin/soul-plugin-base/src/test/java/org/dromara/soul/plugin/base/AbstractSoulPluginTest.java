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

package org.dromara.soul.plugin.base;

import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.SelectorTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.base.cache.BaseDataCache;
import org.junit.Before;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import org.junit.Test;

import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Abstract soul plugin test.
 *
 * @author zhanglei
 */
public final class AbstractSoulPluginTest {

    private RuleData ruleData;

    private PluginData pluginData;

    private SelectorData selectorData;

    private ConditionData conditionData;

    private ServerWebExchange exchange;

    private TestSoulPlugin testSoulPlugin;

    private SoulPluginChain soulPluginChain;

    @Before
    public void setUp() {
        this.ruleData = RuleData.builder().id("1")
                .selectorId("1").enabled(true)
                .loged(true).sort(1).build();
        this.conditionData = new ConditionData();
        this.conditionData.setOperator("match");
        this.conditionData.setParamName("/");
        this.conditionData.setParamType("uri");
        this.conditionData.setParamValue("/http/**");
        this.soulPluginChain = mock(SoulPluginChain.class);
        this.pluginData = PluginData.builder().name("SOUL").enabled(true).build();
        this.selectorData = SelectorData.builder().id("1").pluginName("SOUL")
                .enabled(true).type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).build();
        this.testSoulPlugin = new TestSoulPlugin();
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http/SOUL/SOUL")
                .build());
        when(soulPluginChain.execute(exchange)).thenReturn(Mono.empty());
    }

    /**
     * The plugin is null test.
     */
    @Test
    public void executePluginIsNullTest() {
        StepVerifier.create(testSoulPlugin.execute(exchange, soulPluginChain)).expectSubscription().verifyComplete();
    }

    /**
     * The selector is null test.
     */
    @Test
    public void executeSelectorIsNullTest() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        StepVerifier.create(testSoulPlugin.execute(exchange, soulPluginChain)).expectSubscription().verifyComplete();
    }

    /**
     * The selector data is null test.
     */
    @Test
    public void executeSelectorDataIsNullTest() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        StepVerifier.create(testSoulPlugin.execute(exchange, soulPluginChain)).expectSubscription().verifyComplete();
    }

    /**
     * The rule is null test.
     */
    @Test
    public void executeRuleIsNullTest() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLoged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        StepVerifier.create(testSoulPlugin.execute(exchange, soulPluginChain)).expectSubscription().verifyComplete();
    }

    /**
     * The rule is not null test.
     */
    @Test
    public void executeRuleIsNotNullTest() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLoged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        StepVerifier.create(testSoulPlugin.execute(exchange, soulPluginChain)).expectSubscription().verifyComplete();
    }

    /**
     * The rule is full test.
     */
    @Test
    public void executeRuleFullTest() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(1);
        this.selectorData.setMatchMode(0);
        this.selectorData.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        this.selectorData.setLoged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        StepVerifier.create(testSoulPlugin.execute(exchange, soulPluginChain)).expectSubscription().verifyComplete();
    }

    static class TestSoulPlugin extends AbstractSoulPlugin {

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
            return Mono.empty();
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return "SOUL";
        }
    }
}
