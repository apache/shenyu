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
import org.apache.shenyu.common.dto.convert.rule.RewriteHandle;
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
import static org.junit.jupiter.api.Assertions.assertEquals;

public final class RewritePluginTest extends AbstractPluginDataInit {

    private static final String TEST_REWRITE_PASS = "/http/test/waf/pass";

    @Test
    public void testRewritePlugin() throws IOException, ExecutionException, InterruptedException {
        String pluginResult = initPlugin(PluginEnum.REWRITE.getName(), "");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.REWRITE.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList("", ""));
        assertThat(selectorAndRulesResult, is("success"));
        assertEquals(200, test());
        cleanPluginData(PluginEnum.REWRITE.getName());
    }

    @Test
    public void testReturnNewURIForRewritePlugin() throws IOException, ExecutionException, InterruptedException {
        String pluginResult = initPlugin(PluginEnum.REWRITE.getName(), "");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.REWRITE.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList("/http/test/waf/pass", "/test/waf/deny"));
        assertThat(selectorAndRulesResult, is("success"));
        assertEquals(403, test());
        cleanPluginData(PluginEnum.REWRITE.getName());
    }

    private Integer test() throws ExecutionException, InterruptedException {
        Future<ResultBean> resp = this.getService().submit(() -> HttpHelper.INSTANCE.postGateway(TEST_REWRITE_PASS, ResultBean.class));
        return resp.get().getCode();
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue("/**");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList(final String regex, final String replace) {
        final RuleLocalData ruleLocalData = new RuleLocalData();

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue("/http/test/**");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        ruleLocalData.setRuleName("testRewrite");

        RewriteHandle rewriteHandle = new RewriteHandle();
        rewriteHandle.setRegex(regex);
        rewriteHandle.setReplace(replace);

        ruleLocalData.setRuleHandler(JsonUtils.toJson(rewriteHandle));
        return Collections.singletonList(ruleLocalData);
    }

}
