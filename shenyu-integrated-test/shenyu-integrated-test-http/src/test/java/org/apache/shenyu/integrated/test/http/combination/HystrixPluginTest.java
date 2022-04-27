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

import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.HystrixHandle;
import org.apache.shenyu.common.enums.HystrixIsolationModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.integratedtest.common.result.ResultBean;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.lang.reflect.Type;

import java.util.HashSet;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Stream;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class HystrixPluginTest extends AbstractPluginDataInit {

    private static final String TEST_HYSTRIX_PATH = "/http/test/hystrix/pass";

    private static final String TEST_HYSTRIX_BAD_REQUEST_PATH = "/http/test/hystrix/fallback";

    @BeforeEach
    public void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.HYSTRIX.getName(), "");
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void testPass() throws IOException, ExecutionException, InterruptedException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.HYSTRIX.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList(TEST_HYSTRIX_PATH));
        assertThat(selectorAndRulesResult, is("success"));
        Future<ResultBean> resp = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_HYSTRIX_PATH, ResultBean.class));
        assertEquals(200, resp.get().getCode());
    }

    @Test
    public void testFallbackBySemaphore() throws IOException, ExecutionException, InterruptedException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.HYSTRIX.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList(TEST_HYSTRIX_BAD_REQUEST_PATH));
        assertThat(selectorAndRulesResult, CoreMatchers.is("success"));
        Set<String> resultSet = new HashSet<>();
        Type returnType = new TypeToken<Map<String, Object>>() {
        }.getType();
        Future<Map<String, Object>> resp0 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_HYSTRIX_BAD_REQUEST_PATH, returnType));
        Future<Map<String, Object>> resp1 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_HYSTRIX_BAD_REQUEST_PATH, returnType));
        Future<Map<String, Object>> resp2 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_HYSTRIX_BAD_REQUEST_PATH, returnType));
        Stream.of(resp0.get()).filter(s -> null != s.get("message")).forEach(imp -> {
            resultSet.add(imp.get("message").toString());
        });
        Stream.of(resp1.get()).filter(s -> null != s.get("message")).forEach(imp -> {
            resultSet.add(imp.get("message").toString());
        });
        Stream.of(resp2.get()).filter(s -> null != s.get("message")).forEach(imp -> {
            resultSet.add(imp.get("message").toString());
        });
        assertTrue(resultSet.contains("HystrixPlugin fallback success, please check your service status!"));
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/**");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList(final String commandKey) {
        final RuleLocalData ruleLocalData = new RuleLocalData();
        HystrixHandle hystrixHandle = new HystrixHandle();
        hystrixHandle.setGroupKey("/http/**");
        hystrixHandle.setCommandKey(commandKey);
        hystrixHandle.setMaxConcurrentRequests(1);
        hystrixHandle.setRequestVolumeThreshold(1);
        hystrixHandle.setErrorThresholdPercentage(0);
        hystrixHandle.setSleepWindowInMilliseconds(10000);
        hystrixHandle.setTimeout(120000);
        hystrixHandle.setCallBackUri("/fallback/hystrix");
        hystrixHandle.setExecutionIsolationStrategy(HystrixIsolationModeEnum.SEMAPHORE.getCode());
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue("/http/**");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        ruleLocalData.setRuleHandler(JsonUtils.toJson(hystrixHandle));
        return Collections.singletonList(ruleLocalData);
    }

    @AfterEach
    public void clean() throws IOException {
        cleanPluginData(PluginEnum.HYSTRIX.getName());
    }
}
