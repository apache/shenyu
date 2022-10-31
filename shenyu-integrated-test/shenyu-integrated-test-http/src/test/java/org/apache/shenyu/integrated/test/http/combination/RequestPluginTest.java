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
import org.apache.shenyu.common.dto.convert.rule.RequestHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.HttpCookie;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

public final class RequestPluginTest extends AbstractPluginDataInit {

    @BeforeAll
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.REQUEST.getName(), null);
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.REQUEST.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
    }

    @Test
    public void test() throws IOException {
        Map<String, Object> paramMap = new HashMap<>();

        Map<String, Object> result = HttpHelper.INSTANCE.getFromGateway("/http/test/request/parameter/pass?requestParameter=NULL", paramMap, Map.class);
        assertNotNull(result);
        assertEquals("pass", result.get("msg"));
        assertEquals("requestParameter", GsonUtils.getInstance().convertToMap(String.valueOf(result.get("data"))).get("requestParameter"));

        paramMap.clear();
        paramMap.put("requestHeader", "NULL");
        result = HttpHelper.INSTANCE.getFromGateway("/http/test/request/header/pass", paramMap, Map.class);
        assertNotNull(result);
        assertEquals("pass", result.get("msg"));
        assertEquals("requestHeader", GsonUtils.getInstance().convertToMap(String.valueOf(result.get("data"))).get("requestHeader"));

        paramMap.clear();
        paramMap.put("cookie", new HttpCookie("cookie", "NULL"));
        result = HttpHelper.INSTANCE.getFromGateway("/http/test/request/cookie/pass", paramMap, Map.class);
        assertNotNull(result);
        assertEquals("pass", result.get("msg"));
        assertEquals("cookie", GsonUtils.getInstance().convertToMap(String.valueOf(result.get("data"))).get("cookie"));
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/test/request/**");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList() {
        List<RuleLocalData> ruleLocalDataList = new ArrayList<>();
        ruleLocalDataList.add(buildRuleLocalData());
        return ruleLocalDataList;
    }

    private static RuleLocalData buildRuleLocalData() {
        final RuleLocalData ruleLocalData = new RuleLocalData();
        RequestHandle requestHandle = new RequestHandle();
        final RequestHandle.ShenyuRequestParameter requestParameter = requestHandle.new ShenyuRequestParameter();
        final RequestHandle.ShenyuRequestHeader requestHeader = requestHandle.new ShenyuRequestHeader();
        final RequestHandle.ShenyuCookie cookie = requestHandle.new ShenyuCookie();

        Map<String, String> paramMap = new HashMap<>();
        paramMap.put("requestParameter", "requestParameter");
        requestParameter.setSetParameters(new HashMap<>(paramMap));
        requestHandle.setParameter(requestParameter);

        paramMap.clear();
        paramMap.put("requestHeader", "requestHeader");
        requestHeader.setSetHeaders(new HashMap<>(paramMap));
        requestHandle.setHeader(requestHeader);

        paramMap.clear();
        paramMap.put("cookie", "cookie");
        cookie.setSetCookies(new HashMap<>(paramMap));
        requestHandle.setCookie(cookie);

        ruleLocalData.setRuleHandler(JsonUtils.toJson(requestHandle));
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/test/request/**");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return ruleLocalData;
    }

    @AfterAll
    public static void clean() throws IOException {
        cleanPluginData(PluginEnum.REQUEST.getName());
    }
}
