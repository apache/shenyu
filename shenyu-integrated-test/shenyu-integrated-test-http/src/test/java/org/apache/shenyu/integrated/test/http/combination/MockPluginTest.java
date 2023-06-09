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
import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.MockHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.apache.shenyu.integratedtest.common.utils.ConfUtils.ruleLocalData;
import static org.apache.shenyu.integratedtest.common.utils.ConfUtils.singletonURIEqConditionList;
import static org.apache.shenyu.integratedtest.common.utils.ConfUtils.singletonURIMatchConditionList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThanOrEqualTo;

public class MockPluginTest extends AbstractPluginDataInit {

    private static final String TEST_FIXED_MOCK = "/http/mock/fix";

    private static final String TEST_PLACEHOLDER_MOCK = "/http/mock/placeholder";

    private static final List<ConditionData> SELECTOR_CONDITION_LIST = singletonURIMatchConditionList("/http/mock/**");

    @BeforeEach
    public void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.MOCK.getName(), "");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.MOCK.getName(), "", SELECTOR_CONDITION_LIST, buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
    }

    @Test
    public void testFixContentMock() throws IOException {
        Map<String, Object> correctResponse = HttpHelper.INSTANCE.getFromGateway(TEST_FIXED_MOCK, new HashMap<>(), Map.class);
        assertThat(correctResponse.get("user"),
                is("test"));
    }

    @Test
    public void testPlaceholderContentMock() throws IOException {
        JsonObject correctResponse = HttpHelper.INSTANCE.getFromGateway(TEST_PLACEHOLDER_MOCK, new HashMap<>(), JsonObject.class);
        assertThat(correctResponse.get("number").getAsInt(),
                allOf(greaterThanOrEqualTo(10), lessThanOrEqualTo(20)));
    }

    private static List<RuleLocalData> buildRuleLocalDataList() {

        MockHandle fixMockHandle = buildMockHandle(200, "{\"user\":\"test\"}");
        RuleLocalData fixMockRule = ruleLocalData(JsonUtils.toJson(fixMockHandle),
                singletonURIEqConditionList(TEST_FIXED_MOCK));

        MockHandle placeholderMockHandle = buildMockHandle(200, "{\"number\":${expression|#int(10,20)}}");
        RuleLocalData placeholderMockRule = ruleLocalData(JsonUtils.toJson(placeholderMockHandle),
                singletonURIEqConditionList(TEST_PLACEHOLDER_MOCK));

        return Lists.newArrayList(fixMockRule, placeholderMockRule);
    }

    private static MockHandle buildMockHandle(final Integer httpStatusCode, final String responseContent) {
        MockHandle fixMockHandle = new MockHandle();
        fixMockHandle.setHttpStatusCode(httpStatusCode);
        fixMockHandle.setResponseContent(responseContent);
        return fixMockHandle;
    }

    @AfterEach
    public void clean() throws IOException {
        cleanPluginData(PluginEnum.MOCK.getName());
    }
}
