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

package org.apache.shenyu.integrated.test.http.combination;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.SentinelHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.integratedtest.common.result.ResultBean;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;

import com.google.common.collect.Lists;
import com.google.gson.reflect.TypeToken;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

public final class SentinelPluginTest extends AbstractPluginDataInit {

    private static final String TEST_SENTINEL_PATH = "/http/test/sentinel/pass";

    private static final String TEST_SENTINEL_FALLBACK_PATH = "/http/test/request/accepted";

    @BeforeEach
    public void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.SENTINEL.getName(), "{\"model\":\"black\"}");
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void test() throws IOException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.SENTINEL.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList(null));
        assertThat(selectorAndRulesResult, is("success"));

        Type returnType = new TypeToken<Map<String, Object>>() {
        }.getType();
        Map<String, Object> result = HttpHelper.INSTANCE.postGateway(TEST_SENTINEL_PATH, returnType);
        assertNotNull(result);
        assertEquals("pass", result.get("msg"));
        result = HttpHelper.INSTANCE.postGateway(TEST_SENTINEL_PATH, returnType);
        assertEquals("You have been restricted, please try again later!", result.get("message"));
    }

    @Test
    public void testFallbackUri() throws IOException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.SENTINEL.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList(TEST_SENTINEL_FALLBACK_PATH));
        assertThat(selectorAndRulesResult, is("success"));

        Type returnType = new TypeToken<Map<String, Object>>() {
        }.getType();
        Map<String, Object> result = HttpHelper.INSTANCE.postGateway(TEST_SENTINEL_PATH, returnType);
        assertNotNull(result);
        assertEquals("pass", result.get("msg"));
        ResultBean fallbackRet = HttpHelper.INSTANCE.postGateway(TEST_SENTINEL_PATH, ResultBean.class);
        assertEquals(202, fallbackRet.getCode());
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/test/sentinel/**");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList(final String fallbackUri) {
        final RuleLocalData ruleLocalData = new RuleLocalData();
        SentinelHandle sentinelHandle = new SentinelHandle();
        sentinelHandle.setDegradeRuleCount(1d);
        sentinelHandle.setDegradeRuleEnable(1);
        sentinelHandle.setFlowRuleEnable(1);
        sentinelHandle.setFlowRuleCount(1);
        sentinelHandle.setFlowRuleGrade(1);
        sentinelHandle.setFlowRuleControlBehavior(0);
        sentinelHandle.setDegradeRuleGrade(1);
        sentinelHandle.setDegradeRuleTimeWindow(1);
        sentinelHandle.setDegradeRuleMinRequestAmount(1);
        sentinelHandle.setDegradeRuleStatIntervals(1);
        sentinelHandle.setDegradeRuleSlowRatioThreshold(0.5d);
        sentinelHandle.setFallbackUri(fallbackUri);

        ruleLocalData.setRuleHandler(JsonUtils.toJson(sentinelHandle));
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(TEST_SENTINEL_PATH);
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));

        return Lists.newArrayList(ruleLocalData);
    }

    @AfterEach
    public void clean() throws IOException {
        cleanPluginData(PluginEnum.SENTINEL.getName());
    }
}
