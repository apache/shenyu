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
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.Resilience4JHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.integratedtest.common.result.ResultBean;
import org.apache.shenyu.web.controller.LocalPluginController;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class Resilience4JPluginTest extends AbstractPluginDataInit {

    private static final String TEST_RESILIENCE4J_SUCCESS_OUT_SCOPE_PATH = "/http/test/success";

    private static final String TEST_RESILIENCE4J_SUCCESS_PATH = "/http/test/request/accepted";

    private static final String TEST_RESILIENCE4J_BAD_REQUEST_PATH = "/http/test/request/badrequest";

    @BeforeEach
    public void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.RESILIENCE4J.getName(), null);
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void testRateLimiterPass() throws InterruptedException, ExecutionException, IOException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.RESILIENCE4J.getName(), "",
                        buildSelectorConditionList(), buildRuleLocalDataList(0, 3, null));
        assertThat(selectorAndRulesResult, is("success"));

        Set<Integer> resultSet = new HashSet<>();
        Future<ResultBean> resp1 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_SUCCESS_PATH, ResultBean.class));
        Future<ResultBean> resp2 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_SUCCESS_PATH, ResultBean.class));
        Future<ResultBean> resp3 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_SUCCESS_PATH, ResultBean.class));
        resultSet.add(resp1.get().getCode());
        resultSet.add(resp2.get().getCode());
        resultSet.add(resp3.get().getCode());
        assertTrue(resultSet.contains(202));
    }

    @Test
    public void testRateLimiter() throws InterruptedException, ExecutionException, IOException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.RESILIENCE4J.getName(), "",
                        buildSelectorConditionList(), buildRuleLocalDataList(0, 1, null));
        assertThat(selectorAndRulesResult, is("success"));

        Set<Integer> resultSet = new HashSet<>();
        Future<ResultBean> resp1 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_SUCCESS_PATH, ResultBean.class));
        Future<ResultBean> resp2 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_SUCCESS_PATH, ResultBean.class));
        Future<ResultBean> resp3 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_SUCCESS_PATH, ResultBean.class));
        resultSet.add(resp1.get().getCode());
        resultSet.add(resp2.get().getCode());
        resultSet.add(resp3.get().getCode());
        assertTrue(resultSet.contains(202));
        assertTrue(resultSet.contains(429));
    }

    @Test
    public void testCircuitBreaker() throws IOException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.RESILIENCE4J.getName(), "",
                        buildSelectorConditionList(), buildRuleLocalDataList(1, 5000, null));
        assertThat(selectorAndRulesResult, is("success"));

        List<Integer> rets = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            ResultBean resp = HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_SUCCESS_PATH, ResultBean.class);
            rets.add(resp.getCode());
        }
        for (int i = 0; i < 5; i++) {
            ResultBean resp = HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_BAD_REQUEST_PATH, ResultBean.class);
            rets.add(resp.getCode());
        }
        assertTrue(rets.contains(202));
        assertTrue(rets.contains(400));
        assertTrue(rets.contains(-103));
    }

    @Test
    public void testCircuitBreakerFallbackUri() throws IOException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.RESILIENCE4J.getName(), "",
                        buildSelectorConditionList(), buildRuleLocalDataList(1, 5000, TEST_RESILIENCE4J_SUCCESS_OUT_SCOPE_PATH));
        assertThat(selectorAndRulesResult, is("success"));

        List<Integer> rets = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            ResultBean resp = HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_SUCCESS_PATH, ResultBean.class);
            rets.add(resp.getCode());
        }
        for (int i = 0; i < 5; i++) {
            ResultBean resp = HttpHelper.INSTANCE.getFromGateway(TEST_RESILIENCE4J_BAD_REQUEST_PATH, ResultBean.class);
            rets.add(resp.getCode());
        }
        assertTrue(rets.contains(202));
        assertTrue(rets.contains(400));
        assertTrue(rets.contains(200));
    }

    @AfterEach
    public void clean() throws IOException {
        cleanPluginData(PluginEnum.RESILIENCE4J.getName());
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/test/**");
        return Collections.singletonList(conditionData);
    }

    private static List<LocalPluginController.RuleLocalData> buildRuleLocalDataList(final int circuitEnable, final int limitForPeriod, final String fallbackUri) {
        final LocalPluginController.RuleLocalData ruleLocalData = new LocalPluginController.RuleLocalData();
        Resilience4JHandle resilience4JHandle = new Resilience4JHandle();
        // set parameters for rate limiter
        resilience4JHandle.setTimeoutDuration(5000);
        resilience4JHandle.setLimitRefreshPeriod(500000);
        resilience4JHandle.setLimitForPeriod(limitForPeriod);

        // set parameters for circuit breaker
        resilience4JHandle.setCircuitEnable(circuitEnable);
        resilience4JHandle.setFallbackUri(fallbackUri);
        resilience4JHandle.setMinimumNumberOfCalls(1);
        resilience4JHandle.setFailureRateThreshold(50);

        ruleLocalData.setRuleHandler(JsonUtils.toJson(resilience4JHandle));

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/test/request/**");

        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));

        return Lists.newArrayList(ruleLocalData);
    }

}
