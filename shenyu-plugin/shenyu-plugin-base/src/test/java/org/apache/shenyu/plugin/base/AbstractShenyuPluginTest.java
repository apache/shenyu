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

package org.apache.shenyu.plugin.base;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.enums.TrieMatchModeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.trie.ShenyuTrie;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The Abstract shenyu plugin test.
 */
public final class AbstractShenyuPluginTest {

    private RuleData ruleData;

    private PluginData pluginData;

    private SelectorData selectorData;

    private ConditionData conditionData;

    private ServerWebExchange exchange;

    private TestShenyuPlugin testShenyuPlugin;

    private ShenyuPluginChain shenyuPluginChain;

    @BeforeEach
    public void setUp() {
        mockShenyuConfig();
        this.ruleData = RuleData.builder()
                .id("1")
                .pluginName("SHENYU")
                .selectorId("1")
                .enabled(true)
                .loged(true)
                .matchRestful(false)
                .sort(1).build();
        this.conditionData = new ConditionData();
        this.conditionData.setOperator("match");
        this.conditionData.setParamName("/");
        this.conditionData.setParamType("uri");
        this.conditionData.setParamValue("/http/**");
        this.shenyuPluginChain = mock(ShenyuPluginChain.class);
        this.pluginData = PluginData.builder()
                .name("SHENYU")
                .enabled(true).build();
        this.selectorData = SelectorData.builder()
                .id("1").pluginName("SHENYU")
                .enabled(true)
                .matchRestful(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).build();
        this.testShenyuPlugin = spy(new TestShenyuPlugin());
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http/SHENYU/SHENYU")
                .build());
        clearCache();
        when(shenyuPluginChain.execute(exchange)).thenReturn(Mono.empty());
    }

    /**
     * The plugin is null test.
     */
    @Test
    public void executePluginIsNullTest() {
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /**
     * The selector is null test.
     */
    @Test
    public void executeSelectorIsNullTest() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /**
     * The selector data is null test.
     */
    @Test
    public void executeSelectorDataIsNullTest() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /**
     * The rule is null test.
     */
    @Test
    public void executeRuleIsNullTest() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(true);
        this.selectorData.setMatchRestful(false);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /**
     * The rule is not null test.
     */
    @Test
    public void executeRuleIsNotNullTest() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.ruleData.setMatchRestful(false);
        this.selectorData.setMatchMode(0);
        this.selectorData.setMatchRestful(false);
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    @Test
    public void executeSelectorManyMatch() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.selectorData.setSort(1);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheSelectData(SelectorData.builder()
                .id("2").pluginName("SHENYU")
                .enabled(true)
                .matchMode(0)
                .logged(true)
                .sort(2)
                .conditionList(conditionDataList)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).build());
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    @Test
    public void executeRuleManyMatch() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.ruleData.setMatchRestful(false);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);

        BaseDataCache.getInstance().cacheRuleData(RuleData.builder()
                .id("1")
                .pluginName("SHENYU")
                .selectorId("1")
                .enabled(true)
                .loged(true)
                .matchMode(0)
                .matchRestful(false)
                .conditionDataList(Collections.singletonList(conditionData))
                .sort(1).build());

        BaseDataCache.getInstance().cacheRuleData(RuleData.builder()
                .id("2")
                .pluginName("SHENYU")
                .selectorId("1")
                .enabled(true)
                .loged(true)
                .matchMode(0)
                .matchRestful(false)
                .conditionDataList(Collections.singletonList(conditionData))
                .sort(2).build());
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    /**
     * The rule is full test.
     */
    @Test
    public void executeRuleFullTest() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(1);
        this.ruleData.setMatchRestful(false);
        this.selectorData.setMatchMode(0);
        this.selectorData.setMatchRestful(false);
        this.selectorData.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    private void mockShenyuConfig() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuConfig.class)).thenReturn(new ShenyuConfig());
        when(context.getBean(ShenyuTrie.class)).thenReturn(new ShenyuTrie(100L, 100L, 100L, TrieMatchModeEnum.ANT_PATH_MATCH.getMatchMode()));
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    private void clearCache() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
    }

    static class TestShenyuPlugin extends AbstractShenyuPlugin {

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
            return Mono.empty();
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return "SHENYU";
        }
    }
}
