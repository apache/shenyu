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
import org.apache.shenyu.common.dto.convert.rule.WafHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.WafEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

public final class WafPluginTest extends AbstractPluginDataInit {
    
    @BeforeClass
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.WAF.getName(), "{\"model\":\"black\"}");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.WAF.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
    }

    @Test
    public void test() throws IOException {
        Map<String, Object> result = HttpHelper.INSTANCE.postGateway("/http/test/waf/pass", "", Map.class);
        assertNotNull(result);
        assertEquals("pass", result.get("msg"));
        result = HttpHelper.INSTANCE.postGateway("/http/test/waf/deny", "", Map.class);
        assertNotNull(result);
        assertThat(String.valueOf(result.get("message")), containsString("You are forbidden to visit"));
    }
    
    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/test/waf/**");
        return Collections.singletonList(conditionData);
    }
    
    private static List<RuleLocalData> buildRuleLocalDataList() {
        List<RuleLocalData> ruleLocalDataList = new ArrayList<>();
        ruleLocalDataList.add(buildRuleLocalData(WafEnum.ALLOW.getName(), "200", "/http/test/waf/pass"));
        ruleLocalDataList.add(buildRuleLocalData(WafEnum.REJECT.getName(), "403", "/http/test/waf/deny"));
        return ruleLocalDataList;
    }
    
    private static RuleLocalData buildRuleLocalData(final String permission, final String statusCode, final String paramValue) {
        RuleLocalData ruleLocalData = new RuleLocalData();
        WafHandle wafHandle = new WafHandle();
        wafHandle.setPermission(permission);
        wafHandle.setStatusCode(statusCode);
        ruleLocalData.setRuleHandler(JsonUtils.toJson(wafHandle));
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(paramValue);
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return ruleLocalData;
    }
    
    @AfterClass
    public static void clean() throws IOException {
        cleanPluginData(PluginEnum.WAF.getName());
    }
}
