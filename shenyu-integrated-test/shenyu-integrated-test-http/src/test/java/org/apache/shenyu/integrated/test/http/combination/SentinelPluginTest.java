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
import org.apache.shenyu.common.dto.convert.rule.SentinelHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public final class SentinelPluginTest extends AbstractPluginDataInit {

    private static final int SENTINEL_REQUEST_BURST_SIZE = 6;

    private static final int SENTINEL_REQUEST_ATTEMPTS = 5;

    private static final long SENTINEL_REQUEST_ATTEMPT_INTERVAL_MILLIS = 200L;

    private static final String TEST_SENTINEL_PATH = "/http/test/sentinel/pass";

    private static final String TEST_SENTINEL_FALLBACK_PATH = "fallback:/fallback/sentinel";

    private static final String TEST_SENTINEL_RESTRICTED_MESSAGE = "You have been restricted, please try again later!";

    @BeforeEach
    public void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.SENTINEL.getName(), "{\"model\":\"black\"}");
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void test() throws IOException, ExecutionException, InterruptedException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.SENTINEL.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList(null));
        assertThat(selectorAndRulesResult, is("success"));

        Type returnType = new TypeToken<Map<String, Object>>() {
        }.getType();
        Set<String> messages = collectMessagesUntil(returnType, "pass", TEST_SENTINEL_RESTRICTED_MESSAGE);
        assertThat(messages.contains("pass"), is(true));
        assertThat(messages.contains(TEST_SENTINEL_RESTRICTED_MESSAGE), is(true));
    }

    @Test
    public void testFallbackUri() throws IOException, ExecutionException, InterruptedException {
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.SENTINEL.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList(TEST_SENTINEL_FALLBACK_PATH));
        assertThat(selectorAndRulesResult, is("success"));

        Type returnType = new TypeToken<Map<String, Object>>() {
        }.getType();
        List<Map<String, Object>> results = collectResultsUntil(returnType, "pass", ShenyuResultEnum.SENTINEL_PLUGIN_FALLBACK.getMsg());
        Set<String> messages = toMessages(results);
        Set<Integer> codes = toCodes(results);
        assertThat(messages.contains("pass"), is(true));
        assertThat(codes.contains(ShenyuResultEnum.SENTINEL_PLUGIN_FALLBACK.getCode()), is(true));
        assertThat(messages.contains(ShenyuResultEnum.SENTINEL_PLUGIN_FALLBACK.getMsg()), is(true));
    }

    private List<Map<String, Object>> collectResultsUntil(final Type returnType,
                                                          final String... expectedMessages)
            throws ExecutionException, InterruptedException {
        List<Map<String, Object>> results = new ArrayList<>();
        Set<String> expected = new HashSet<>(Arrays.asList(expectedMessages));
        for (int attempt = 0; attempt < SENTINEL_REQUEST_ATTEMPTS; attempt++) {
            results.addAll(postSentinelRequestBurst(returnType));
            if (toMessages(results).containsAll(expected)) {
                return results;
            }
            Thread.sleep(SENTINEL_REQUEST_ATTEMPT_INTERVAL_MILLIS);
        }
        return results;
    }

    private Set<String> collectMessagesUntil(final Type returnType, final String... expectedMessages)
            throws ExecutionException, InterruptedException {
        return toMessages(collectResultsUntil(returnType, expectedMessages));
    }

    private List<Map<String, Object>> postSentinelRequestBurst(final Type returnType)
            throws ExecutionException, InterruptedException {
        List<Future<Map<String, Object>>> futures = new ArrayList<>();
        for (int i = 0; i < SENTINEL_REQUEST_BURST_SIZE; i++) {
            futures.add(this.getService().submit(() -> HttpHelper.INSTANCE.postGateway(TEST_SENTINEL_PATH, returnType)));
        }
        List<Map<String, Object>> results = new ArrayList<>();
        for (Future<Map<String, Object>> future : futures) {
            results.add(future.get());
        }
        return results;
    }

    private static Set<String> toMessages(final List<Map<String, Object>> results) {
        Set<String> messages = new HashSet<>();
        for (Map<String, Object> result : results) {
            assertNotNull(result);
            Object msg = result.containsKey("msg") ? result.get("msg") : result.get("message");
            if (msg instanceof String) {
                messages.add((String) msg);
            }
        }
        return messages;
    }

    private static Set<Integer> toCodes(final List<Map<String, Object>> results) {
        Set<Integer> codes = new HashSet<>();
        for (Map<String, Object> result : results) {
            assertNotNull(result);
            Object code = result.get("code");
            if (code instanceof Number) {
                codes.add(((Number) code).intValue());
            }
        }
        return codes;
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
        sentinelHandle.setFlowRuleMaxQueueingTimeMs(500);
        sentinelHandle.setFlowRuleWarmUpPeriodSec(10);

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
