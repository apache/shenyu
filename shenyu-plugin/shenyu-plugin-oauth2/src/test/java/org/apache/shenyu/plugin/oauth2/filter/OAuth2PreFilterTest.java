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

package org.apache.shenyu.plugin.oauth2.filter;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.mock;

@ExtendWith(MockitoExtension.class)
public class OAuth2PreFilterTest {

    @Mock
    private OAuth2PreFilter preFilter;

    @BeforeEach
    public void setup() {
        preFilter = new OAuth2PreFilter(mock(List.class));
    }

    @Test
    public void testFilter() {
        final MockServerWebExchange mockServerWebExchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        final Mono<Void> voidMono = preFilter.filter(mockServerWebExchange, filterExchange -> Mono.empty());
        StepVerifier.create(voidMono).expectSubscription().verifyComplete();
        PluginData pluginData = new PluginData();
        pluginData.setName(PluginEnum.OAUTH2.getName());
        pluginData.setEnabled(true);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        final Mono<Void> voidMono2 = preFilter.filter(mockServerWebExchange, filterExchange -> Mono.empty());
        StepVerifier.create(voidMono2).expectSubscription().verifyComplete();
    }

    @Test
    public void processPathMatchersTest() throws NoSuchMethodException {
        final Method processPathMatchers = OAuth2PreFilter.class.getDeclaredMethod("processPathMatchers", ServerWebExchange.class);
        processPathMatchers.setAccessible(true);
        final MockServerWebExchange mockServerWebExchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http/text").build());
        Assertions.assertDoesNotThrow(() -> processPathMatchers.invoke(preFilter, mockServerWebExchange));
        mockServerWebExchange.getAttributes().put("enable", true);
        Assertions.assertDoesNotThrow(() -> processPathMatchers.invoke(preFilter, mockServerWebExchange));
        SelectorData selectorData1 = new SelectorData();
        selectorData1.setPluginName(PluginEnum.OAUTH2.getName());
        selectorData1.setEnabled(true);
        selectorData1.setSort(1);
        selectorData1.setId("selectorData1");
        selectorData1.setType(SelectorTypeEnum.CUSTOM_FLOW.getCode());
        BaseDataCache.getInstance().cacheSelectData(selectorData1);
        SelectorData selectorData2 = new SelectorData();
        selectorData2.setPluginName(PluginEnum.OAUTH2.getName());
        selectorData2.setEnabled(false);
        selectorData2.setSort(1);
        selectorData2.setId("selectorData2");
        BaseDataCache.getInstance().cacheSelectData(selectorData2);
        SelectorData selectorData3 = new SelectorData();
        selectorData3.setPluginName(PluginEnum.OAUTH2.getName());
        selectorData3.setEnabled(true);
        selectorData3.setSort(1);
        selectorData3.setId("selectorData3");
        selectorData3.setSort(1);
        selectorData3.setType(SelectorTypeEnum.CUSTOM_FLOW.getCode());
        BaseDataCache.getInstance().cacheSelectData(selectorData3);
        Assertions.assertDoesNotThrow(() -> processPathMatchers.invoke(preFilter, mockServerWebExchange));
        SelectorData selectorData4 = new SelectorData();
        selectorData4.setPluginName(PluginEnum.OAUTH2.getName());
        selectorData4.setEnabled(true);
        selectorData4.setSort(1);
        selectorData4.setId("selectorData4");
        selectorData4.setType(SelectorTypeEnum.CUSTOM_FLOW.getCode());
        selectorData4.setMatchMode(MatchModeEnum.OR.getCode());
        selectorData4.setConditionList(buildConditionDataList("match", "/", "uri", "/http/**"));
        BaseDataCache.getInstance().cacheSelectData(selectorData4);

        RuleData ruleData = new RuleData();
        ruleData.setSelectorId(selectorData4.getId());
        ruleData.setEnabled(true);
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.POST.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamName("post");
        conditionData.setParamValue("POST");
        ruleData.setConditionDataList(Collections.singletonList(conditionData));
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        Assertions.assertDoesNotThrow(() -> processPathMatchers.invoke(preFilter, mockServerWebExchange));
        selectorData1.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        Assertions.assertDoesNotThrow(() -> processPathMatchers.invoke(preFilter, mockServerWebExchange));
    }

    private List<ConditionData> buildConditionDataList(final String operator, final String paramName,
                                                       final String paramType, final String paramValue) {
        ConditionData conditionData = new ConditionData();
        conditionData.setOperator(operator);
        conditionData.setParamName(paramName);
        conditionData.setParamType(paramType);
        conditionData.setParamValue(paramValue);
        List<ConditionData> conditionList = new ArrayList<>();
        conditionList.add(conditionData);
        return conditionList;
    }
}
