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

import com.google.common.collect.Lists;
import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.Resilience4JHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.UserDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public final class Resilience4JPluginTest extends AbstractPluginDataInit {

    private static final String TEST_RESILIENCE4J_PATH = "/http/test/path/123?name=joker";

    @BeforeAll
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.RESILIENCE4J.getName(), null);
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.RESILIENCE4J.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList(0));
        assertThat(selectorAndRulesResult, is("success"));
    }

    @Test
    public void testCircuitBreaker() throws IOException {
        UserDTO result = HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_PATH, UserDTO.class);
        assertNotNull(result);
        assertEquals("123", result.getUserId());
        result = HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_PATH, UserDTO.class);
        assertEquals("You have been restricted, please try again later!", result.getUserId());
    }

    @Test
    @Disabled
    public void testRateLimiter() throws IOException {
        Type returnType = new TypeToken<Map<String, Object>>() {
        }.getType();
        Map<String, Object> result = HttpHelper.INSTANCE.postGateway(TEST_RESILIENCE4J_PATH, returnType);
        assertNotNull(result);
        assertEquals("pass", result.get("msg"));
        result = HttpHelper.INSTANCE.postGateway(TEST_RESILIENCE4J_PATH, returnType);
        assertEquals("You have been restricted, please try again later!", result.get("message"));
    }

    @Test
    @Disabled
    public void testCombined() throws IOException {
        Type returnType = new TypeToken<Map<String, Object>>() {
        }.getType();
        Map<String, Object> result = HttpHelper.INSTANCE.postGateway(TEST_RESILIENCE4J_PATH, returnType);
        assertNotNull(result);
        assertEquals("pass", result.get("msg"));
        result = HttpHelper.INSTANCE.postGateway(TEST_RESILIENCE4J_PATH, returnType);
        assertEquals("You have been restricted, please try again later!", result.get("message"));
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/test/**");
        return Collections.singletonList(conditionData);
    }

    private static List<LocalPluginController.RuleLocalData> buildRuleLocalDataList(final int circuitEnable) {
        final LocalPluginController.RuleLocalData ruleLocalData = new LocalPluginController.RuleLocalData();
        Resilience4JHandle resilience4JHandle = new Resilience4JHandle();
        resilience4JHandle.setCircuitEnable(circuitEnable);
        resilience4JHandle.setTimeoutDuration(5000);
        resilience4JHandle.setLimitRefreshPeriod(50000);
        resilience4JHandle.setLimitForPeriod(1);

        ruleLocalData.setRuleHandler(JsonUtils.toJson(resilience4JHandle));

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/test/**");

        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));

        return Lists.newArrayList(ruleLocalData);
    }

}
