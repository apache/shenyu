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

package org.apache.shenyu.web.controller;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.web.controller.PluginController.SelectorRuleData;
import org.junit.Test;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type Plugin controller Test.
 */
public class PluginControllerTest {
    
    @Test
    public void testSelectorRuleData() {
        List<DivideUpstream> collect = Stream.of(1).map(weight -> DivideUpstream.builder().upstreamUrl("127.0.0.1:8089").build()).collect(Collectors.toList());
        SelectorRuleData selectorRuleData = new SelectorRuleData();
        selectorRuleData.setPluginName(PluginEnum.DIVIDE.getName());
        selectorRuleData.setSelectorHandler(JsonUtils.toJson(collect));
        List<ConditionData> dataList = Stream.of(1).map(weight -> {
            ConditionData data = new ConditionData();
            data.setParamType(ParamTypeEnum.URI.getName());
            data.setOperator(OperatorEnum.MATCH.getAlias());
            data.setParamValue("/**");
            return data;
        }).collect(Collectors.toList());
        selectorRuleData.setConditionDataList(dataList);
        DivideRuleHandle divideRuleHandle = new DivideRuleHandle();
        divideRuleHandle.setLoadBalance(LoadBalanceEnum.RANDOM.getName());
        selectorRuleData.setRuleHandler(JsonUtils.toJson(divideRuleHandle));
    }
}
