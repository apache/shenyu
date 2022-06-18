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
import org.apache.shenyu.common.dto.convert.rule.impl.CacheRuleHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.integratedtest.common.result.ResultBean;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class CachePluginTest extends AbstractPluginDataInit {

    private static final String TEST_CACHE_PATH = "/http/test/cache";

    @Test
    public void testMemoryModle() throws IOException, ExecutionException, InterruptedException {
        String pluginResult = initPlugin(PluginEnum.CACHE.getName(),
                "{\"cacheType\":\"memory\"}");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.CACHE.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
        assertTrue(testPass());
        cleanPluginData(PluginEnum.CACHE.getName());
    }

    @Test
    public void testRedisModle() throws IOException, ExecutionException, InterruptedException {
        String pluginResult = initPlugin(PluginEnum.CACHE.getName(), "{\"cacheType\":\"redis\","
                + "\"database\":\"0\","
                + "\"mode\":\"standalone\","
                + "\"url\":\"shenyu-redis:6379\","
                + "\"password\":\"abc\","
                + "\"maxIdle\":\"8\","
                + "\"minIdle\":\"0\","
                + "\"maxActive\":\"8\","
                + "\"maxWait\":\"-1\"}");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.CACHE.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
        assertTrue(testPass());
        cleanPluginData(PluginEnum.CACHE.getName());
    }

    private boolean testPass() throws ExecutionException, InterruptedException {
        Future<ResultBean> resp0 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_CACHE_PATH, ResultBean.class));
        Thread.sleep(2000);
        Future<ResultBean> resp1 = this.getService().submit(() -> HttpHelper.INSTANCE.getFromGateway(TEST_CACHE_PATH, ResultBean.class));
        return resp0.get().getMsg().equals(resp1.get().getMsg());
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/http/test/cache");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList() {
        final RuleLocalData ruleLocalData = new RuleLocalData();
        CacheRuleHandle cacheRuleHandle = new CacheRuleHandle();
        cacheRuleHandle.setTimeoutSeconds(3000L);
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamName("testCache");
        conditionData.setParamValue("/http/test/cache");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        ruleLocalData.setRuleHandler(JsonUtils.toJson(cacheRuleHandle));
        return Collections.singletonList(ruleLocalData);
    }

}
