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

import org.apache.shenyu.common.constant.Constants;
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
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;

public final class HystrixPluginTest extends AbstractPluginDataInit {

    private static final String TEST_HYSTRIX_PATH = "/http/test/success";

    private static final String TEST_HYSTRIX_BAD_REQUEST_PATH = "/http/test/request/badrequest";

    @BeforeEach
    public void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.HYSTRIX.getName(), "");
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void testPass() throws IOException, ExecutionException, InterruptedException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.HYSTRIX.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList(null));
        assertThat(selectorAndRulesResult, is("success"));
        Future<ResultBean> resp = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_HYSTRIX_PATH, ResultBean.class));
        assertEquals(200, resp.get().getCode());

    }

    @Test
    public void testFallbackUri() throws IOException, ExecutionException, InterruptedException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.HYSTRIX.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList(null));
        assertThat(selectorAndRulesResult, CoreMatchers.is("success"));
        // there have some problems
        Future<ResultBean> resp = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_HYSTRIX_BAD_REQUEST_PATH, ResultBean.class));
        assertEquals(400, resp.get().getCode());
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/http/test/**");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList(final String fallbackUri) {
        final RuleLocalData ruleLocalData = new RuleLocalData();
        HystrixHandle hystrixHandle = new HystrixHandle();
        hystrixHandle.setGroupKey("contextPath");
        hystrixHandle.setCommandKey("commandKey");
        hystrixHandle.setMaxConcurrentRequests(0);
        hystrixHandle.setErrorThresholdPercentage(0);
        hystrixHandle.setRequestVolumeThreshold(0);
        hystrixHandle.setSleepWindowInMilliseconds(1);
        hystrixHandle.setTimeout(Constants.TIME_OUT);
        hystrixHandle.setCallBackUri(TEST_HYSTRIX_BAD_REQUEST_PATH);
        HystrixHandle.HystrixThreadPoolConfig hystrixThreadPoolConfig = new HystrixHandle.HystrixThreadPoolConfig();
        hystrixThreadPoolConfig.setCoreSize(0);
        hystrixThreadPoolConfig.setMaximumSize(0);
        hystrixThreadPoolConfig.setMaxQueueSize(0);
        hystrixHandle.setHystrixThreadPoolConfig(hystrixThreadPoolConfig);
        hystrixHandle.setExecutionIsolationStrategy(HystrixIsolationModeEnum.SEMAPHORE.getCode());
        ruleLocalData.setRuleHandler(JsonUtils.toJson(hystrixHandle));

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/http/test/request/**");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return Collections.singletonList(ruleLocalData);
    }

    @AfterEach
    public void clean() throws IOException {
        cleanPluginData(PluginEnum.HYSTRIX.getName());
    }
}
