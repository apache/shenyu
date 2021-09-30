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

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.HystrixHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;

public final class HystrixPluginTest extends AbstractPluginDataInit {

    @BeforeClass
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.HYSTRIX.getName(), null);
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(
                PluginEnum.HYSTRIX.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
    }

    @Test
    public void test() throws IOException, InterruptedException {

        Map<String, Object> resultPass =
                HttpHelper.INSTANCE.postGateway("/http/test/fault-tolerance/timeout", "", Map.class);
        assertNotNull(resultPass);
        assertEquals("Service call timeout!", resultPass.get("message"));
        Map<String, Object> resultDeny =
                HttpHelper.INSTANCE.postGateway("/http/test/fault-tolerance/timeout", "", Map.class);
        assertNotNull(resultDeny);
        assertEquals("Service invocation exception, or no result is returned!", resultDeny.get("message"));

        // test sleepWindowInMilliseconds
        Awaitility.await().pollDelay(Durations.ONE_SECOND).untilAsserted(() -> {
            Map<String, Object> result =
                    HttpHelper.INSTANCE.postGateway("/http/test/fault-tolerance/timeout", "", Map.class);
            assertNotNull(result);
            assertEquals("Service call timeout!", result.get("message"));
        });

    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/test/fault-tolerance/**");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList() {
        List<RuleLocalData> ruleLocalDataList = new ArrayList<>();
        ruleLocalDataList.add(buildRuleLocalData("/http/test/fault-tolerance/timeout"));
        return ruleLocalDataList;
    }

    private static RuleLocalData buildRuleLocalData(final String paramValue) {

        HystrixHandle hystrixHandle = new HystrixHandle();
        hystrixHandle.setMaxConcurrentRequests(1);
        hystrixHandle.setErrorThresholdPercentage(1);
        hystrixHandle.setRequestVolumeThreshold(1);
        hystrixHandle.setSleepWindowInMilliseconds(1000);
        hystrixHandle.setTimeout(500);

        RuleLocalData ruleLocalData = new RuleLocalData();
        ruleLocalData.setRuleHandler(JsonUtils.toJson(hystrixHandle));

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(paramValue);
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return ruleLocalData;
    }

    @AfterClass
    public static void clean() throws IOException {
        cleanPluginData(PluginEnum.HYSTRIX.getName());
    }
}
