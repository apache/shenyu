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

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.gson.reflect.TypeToken;
import okhttp3.Headers;
import okhttp3.Response;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.impl.ModifyResponseRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.PluginController;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.*;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;
import static org.junit.Assert.assertNotNull;

/**
 * ModifyResponsePluginTest.
 */
public class ModifyResponsePluginTest extends AbstractPluginDataInit {

    private static final String addHeader = "addHeader";
    private static final String setHeadersExist = "setHeadersExist";
    private static final String setHeaderNotExist = "setHeaderNotExist";
    private static final String replaceHeaderKeys = "replaceHeaderKeys";
    private static final String removeHeaderKeys = "removeHeaderKeys";
    private static final String addBodyKeys = "addBodyKeys";
    private static final String originReplaceBodyKeys = "originReplaceBodyKeys";
    private static final String replaceBodyKeys = "replaceBodyKeys";
    private static final String removeBodyKeys = "removeBodyKeys";
    private static final int exceptStatusCode = 201;

    @BeforeClass
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.MODIFY_RESPONSE.getName(), "{\"ruleHandlerPageType\":\"custom\"}");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.MODIFY_RESPONSE.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/test/modifyResponse");
        return Collections.singletonList(conditionData);
    }

    private static List<PluginController.RuleLocalData> buildRuleLocalDataList() {
        List<PluginController.RuleLocalData> ruleLocalDataList = new ArrayList<>();
        ruleLocalDataList.add(buildRuleLocalData());
        return ruleLocalDataList;
    }

    private static PluginController.RuleLocalData buildRuleLocalData() {
        final ModifyResponseRuleHandle modifyResponseRuleHandle = new ModifyResponseRuleHandle();
        modifyResponseRuleHandle.setAddHeaders(ImmutableMap.<String, String>builder().put(addHeader, "true").build());
        modifyResponseRuleHandle.setSetHeaders(ImmutableMap.<String, String>builder().put(setHeadersExist, "false").put(setHeaderNotExist, "true").build());
        modifyResponseRuleHandle.setReplaceHeaderKeys(ImmutableMap.<String, String>builder().put(replaceHeaderKeys, "false").build());
        modifyResponseRuleHandle.setRemoveHeaderKeys(ImmutableSet.<String>builder().add(removeHeaderKeys).build());
        modifyResponseRuleHandle.setStatusCode(exceptStatusCode);
        modifyResponseRuleHandle.setRemoveBodyKeys(ImmutableSet.<String>builder().add(removeBodyKeys).build());

        final ParamMappingHandle.ParamMapInfo addBodyKeysHandler = new ParamMappingHandle.ParamMapInfo();
        addBodyKeysHandler.setPath("$");
        addBodyKeysHandler.setKey(addBodyKeys);
        addBodyKeysHandler.setValue("true");
        modifyResponseRuleHandle.setAddBodyKeys(Collections.singletonList(addBodyKeysHandler));

        final ParamMappingHandle.ParamMapInfo replaceBodyKeysHandler = new ParamMappingHandle.ParamMapInfo();
        replaceBodyKeysHandler.setPath("$");
        replaceBodyKeysHandler.setKey(originReplaceBodyKeys);
        replaceBodyKeysHandler.setValue(replaceBodyKeys);
        modifyResponseRuleHandle.setReplaceBodyKeys(Collections.singletonList(replaceBodyKeysHandler));

        final Set<String> removeBodyKeysHandler = new HashSet<>();
        removeBodyKeysHandler.add("$.removeBodyKeys");
        modifyResponseRuleHandle.setRemoveBodyKeys(removeBodyKeysHandler);

        PluginController.RuleLocalData ruleLocalData = new PluginController.RuleLocalData();
        ruleLocalData.setRuleHandler(JsonUtils.toJson(modifyResponseRuleHandle));
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/http/test/modifyResponse");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return ruleLocalData;
    }

    @AfterClass
    public static void clean() throws IOException {
        cleanPluginData(PluginEnum.MODIFY_RESPONSE.getName());
    }

    @Test
    public void test() throws IOException {
        final Response response = HttpHelper.INSTANCE.getResponseFromGateway("/http/test/modifyResponse", new HashMap<>(0));
        final Type mapType = new TypeToken<Map<String, Boolean>>() {}.getType();
        final Map<String, Boolean> body = GsonUtils.getGson().fromJson(Objects.requireNonNull(response.body()).string(), mapType);
        assertNotNull(response);
        assertEquals(exceptStatusCode, response.code());

        final Headers headers = response.headers();
        assertTrue(Boolean.parseBoolean(headers.get(addHeader)));
        assertFalse(Boolean.parseBoolean(headers.get(setHeadersExist)));
        assertTrue(Boolean.parseBoolean(headers.get(setHeaderNotExist)));
        assertFalse(Boolean.parseBoolean(headers.get(replaceHeaderKeys)));
        assertFalse(headers.toMultimap().containsKey(removeHeaderKeys));

        assertTrue(body.get(addBodyKeys));
        assertNull(body.get(originReplaceBodyKeys));
        assertTrue(body.get(replaceBodyKeys));
    }
}
