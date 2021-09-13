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
import org.apache.shenyu.common.dto.convert.RedirectHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.PluginController.RuleLocalData;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

public final class RedirectPluginTest extends AbstractPluginDataInit {
    
    @BeforeClass
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.REDIRECT.getName(), "");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.REDIRECT.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
    }
    
    @Test
    public void testRedirectPlugin() throws IOException {
        final String redirectUserId = "111";
        final String redirectToInnerPathUrl = "/http/test/path/" + redirectUserId + "?name=redirectToInnerPath";
        Map<String, Object> result = HttpHelper.INSTANCE.getFromGateway(redirectToInnerPathUrl, Map.class);
        assertNotNull(result);
        assertNull(result.get("userId"));
        assertEquals("UP", result.get("status"));

        final String redirectToOuterPathUrl = "/http/test/path/" + redirectUserId + "?name=redirectToOuterPath";
        result = HttpHelper.INSTANCE.getFromGateway(redirectToOuterPathUrl, Map.class);
        assertNotEquals(redirectUserId, result.get("userId"));
        assertEquals("222", result.get("userId"));

        final String notRedirectUserId = "333";
        final String notRedirectUrl = "/http/test/path/" + notRedirectUserId + "?name=notRedirect";
        result = HttpHelper.INSTANCE.getFromGateway(notRedirectUrl, Map.class);
        assertNotNull(result);
        assertEquals(notRedirectUserId, result.get("userId"));
    }
    
    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/http/test/path/111");
        return Collections.singletonList(conditionData);
    }
    
    private static List<RuleLocalData> buildRuleLocalDataList() {
        List<RuleLocalData> ruleLocalDataList = new ArrayList<>();
        ruleLocalDataList.add(buildRuleLocalData("http://localhost:9195/http/test/path/222?name=redirectToOuterPath", "/http/test/path/111", "redirectToOuterPath"));
        ruleLocalDataList.add(buildRuleLocalData("/actuator/health", "/http/test/path/111", "redirectToInnerPath"));
        return ruleLocalDataList;
    }
    
    private static RuleLocalData buildRuleLocalData(final String redirectURI, final String uri, final String query) {
        RuleLocalData ruleLocalData = new RuleLocalData();
        RedirectHandle redirectHandle = new RedirectHandle();
        redirectHandle.setRedirectURI(redirectURI);
        ruleLocalData.setRuleHandler(JsonUtils.toJson(redirectHandle));
        ruleLocalData.setConditionDataList(buildRuleConditionList(uri, query));
        return ruleLocalData;
    }
    
    private static List<ConditionData> buildRuleConditionList(final String uri, final String query) {
        List<ConditionData> results = new ArrayList<>();
        results.add(buildConditionData(ParamTypeEnum.URI.getName(), uri));
        results.add(buildConditionData(ParamTypeEnum.QUERY.getName(), query));
        return results;
    }
    
    private static ConditionData buildConditionData(final String paramType, final String paramValue) {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(paramType);
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamName("name");
        conditionData.setParamValue(paramValue);
        return conditionData;
    }
    
    @AfterClass
    public static void clean() throws IOException {
        cleanPluginData(PluginEnum.REDIRECT.getName());
    }
}
