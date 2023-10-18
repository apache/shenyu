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
import org.apache.shenyu.common.dto.convert.rule.RateLimiterHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.AdminResponse;
import org.apache.shenyu.integratedtest.common.dto.UserDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class RateLimiterPluginTest extends AbstractPluginDataInit {

    @BeforeEach
    public void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.RATE_LIMITER.getName(), "{\"mode\":\"standalone\",\"master\":\"mymaster\",\"url\":\"shenyu-redis:6379\",\"password\":\"abc\"}");
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void testSlidingWindow() throws IOException, ExecutionException, InterruptedException {
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.RATE_LIMITER.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList("slidingWindow"));
        assertThat(selectorAndRulesResult, is("success"));

        Future<UserDTO> allowedRespFuture1 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", UserDTO.class));
        assertEquals("Tom", allowedRespFuture1.get().getUserName());

        Future<AdminResponse<Object>> rejectedRespFuture = this.getService().submit(() ->
                HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", new TypeToken<AdminResponse<Object>>() {
                }.getType()));
        AdminResponse<Object> dto = rejectedRespFuture.get();
        assertEquals("You have been restricted, please try again later!", dto.getMessage());

        Thread.sleep(2000);
        Future<UserDTO> allowedRespFuture2 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", UserDTO.class));
        assertEquals("Tom", allowedRespFuture2.get().getUserName());
    }

    @Test
    public void testLeakyBucket() throws IOException, ExecutionException, InterruptedException {
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.RATE_LIMITER.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList("leakyBucket"));
        assertThat(selectorAndRulesResult, is("success"));

        Future<UserDTO> allowedRespFuture1 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", UserDTO.class));
        assertEquals("Tom", allowedRespFuture1.get().getUserName());

        Future<AdminResponse<Object>> rejectedRespFuture = this.getService().submit(() ->
                HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", new TypeToken<AdminResponse<Object>>() {
                }.getType()));
        AdminResponse<Object> dto = rejectedRespFuture.get();
        assertEquals("You have been restricted, please try again later!", dto.getMessage());

        Thread.sleep(2000);
        Future<UserDTO> allowedRespFuture2 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", UserDTO.class));
        assertEquals("Tom", allowedRespFuture2.get().getUserName());
    }

    @Test
    public void testTokenBucket() throws IOException, ExecutionException, InterruptedException {
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.RATE_LIMITER.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList("tokenBucket"));
        assertThat(selectorAndRulesResult, is("success"));

        Future<UserDTO> allowedRespFuture1 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", UserDTO.class));
        assertEquals("Tom", allowedRespFuture1.get().getUserName());

        Future<AdminResponse<Object>> rejectedRespFuture = this.getService().submit(() ->
                HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", new TypeToken<AdminResponse<Object>>() {
                }.getType()));
        AdminResponse<Object> dto = rejectedRespFuture.get();
        assertEquals("You have been restricted, please try again later!", dto.getMessage());

        Thread.sleep(2000);
        Future<UserDTO> allowedRespFuture2 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", UserDTO.class));
        assertEquals("Tom", allowedRespFuture2.get().getUserName());
    }

    @Test
    public void testConcurrentTokenBucket() throws IOException, ExecutionException, InterruptedException {
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.RATE_LIMITER.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList("concurrent"));
        assertThat(selectorAndRulesResult, is("success"));

        UserDTO allowedResp = HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", UserDTO.class);
        assertEquals("Tom", allowedResp.getUserName());

        List<Future<AdminResponse<Object>>> futures = new ArrayList<>();
        for (int i = 0; i < 4; i++) {
            Future<AdminResponse<Object>> rejectedRespFuture = this.getService().submit(() ->
                    HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", new TypeToken<AdminResponse<Object>>() {
                    }.getType()));
            futures.add(rejectedRespFuture);
        }

        int errorCount = 0;
        int correctCount = 0;
        for (Future<AdminResponse<Object>> future : futures) {
            AdminResponse<Object> adminResponse = future.get();
            if (adminResponse.getCode() != null && adminResponse.getCode() == 429) {
                errorCount++;
            } else {
                correctCount++;
            }
        }
        assertTrue(errorCount > 0);
        assertTrue(correctCount > 0);
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/http/test/path/123");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList(final String algorithmName) {
        final RuleLocalData ruleLocalData = new RuleLocalData();

        RateLimiterHandle rateLimiterHandle = new RateLimiterHandle();
        rateLimiterHandle.setAlgorithmName(algorithmName);
        rateLimiterHandle.setReplenishRate(0.5);
        rateLimiterHandle.setBurstCapacity(1);
        // see WholeKeyResolver.java
        rateLimiterHandle.setKeyResolverName("WHOLE_KEY_RESOLVER");
        ruleLocalData.setRuleHandler(JsonUtils.toJson(rateLimiterHandle));

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/http/test/path/123");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));

        return Collections.singletonList(ruleLocalData);
    }

    @AfterEach
    public void clean() throws IOException {
        String res = cleanPluginData(PluginEnum.RATE_LIMITER.getName());
        assertThat(res, is("success"));
    }
}
