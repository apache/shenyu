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
import com.google.gson.JsonObject;
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.HashSet;
import java.util.Objects;
import java.util.HashMap;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertEquals;

/**
 * ModifyResponsePluginTest.
 */
public class ModifyResponsePluginTest extends AbstractPluginDataInit {

    private static final String ADD_HEADER = "addHeader";

    private static final String SET_HEADERS_EXIST = "setHeadersExist";

    private static final String SET_HEADER_NOT_EXIST = "setHeaderNotExist";

    private static final String REPLACE_HEADER_KEYS = "replaceHeaderKeys";

    private static final String REMOVE_HEADER_KEYS = "removeHeaderKeys";

    private static final String ADD_BODY_KEYS = "addBodyKeys";

    private static final String ORIGIN_REPLACE_BODY_KEYS = "originReplaceBodyKeys";

    private static final String REPLACE_BODY_KEYS = "replaceBodyKeys";

    private static final String REMOVE_BODY_KEYS = "removeBodyKeys";

    private static final int EXCEPT_STATUS_CODE = 201;

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
        modifyResponseRuleHandle.setAddHeaders(ImmutableMap.<String, String>builder().put(ADD_HEADER, "true").build());
        modifyResponseRuleHandle.setSetHeaders(ImmutableMap.<String, String>builder().put(SET_HEADERS_EXIST, "false").put(SET_HEADER_NOT_EXIST, "true").build());
        modifyResponseRuleHandle.setReplaceHeaderKeys(ImmutableMap.<String, String>builder().put(REPLACE_HEADER_KEYS, "false").build());
        modifyResponseRuleHandle.setRemoveHeaderKeys(ImmutableSet.<String>builder().add(REMOVE_HEADER_KEYS).build());
        modifyResponseRuleHandle.setStatusCode(EXCEPT_STATUS_CODE);
        modifyResponseRuleHandle.setRemoveBodyKeys(ImmutableSet.<String>builder().add(REMOVE_BODY_KEYS).build());

        final ParamMappingHandle.ParamMapInfo addBodyKeysHandler = new ParamMappingHandle.ParamMapInfo();
        addBodyKeysHandler.setPath("$");
        addBodyKeysHandler.setKey(ADD_BODY_KEYS);
        addBodyKeysHandler.setValue("true");
        modifyResponseRuleHandle.setAddBodyKeys(Collections.singletonList(addBodyKeysHandler));

        final ParamMappingHandle.ParamMapInfo replaceBodyKeysHandler = new ParamMappingHandle.ParamMapInfo();
        replaceBodyKeysHandler.setPath("$");
        replaceBodyKeysHandler.setKey(ORIGIN_REPLACE_BODY_KEYS);
        replaceBodyKeysHandler.setValue(REPLACE_BODY_KEYS);
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
        assertNotNull(response);
        assertEquals(EXCEPT_STATUS_CODE, response.code());

        final Headers headers = response.headers();
        assertTrue(Boolean.parseBoolean(headers.get(ADD_HEADER)));
        assertFalse(Boolean.parseBoolean(headers.get(SET_HEADERS_EXIST)));
        assertTrue(Boolean.parseBoolean(headers.get(SET_HEADER_NOT_EXIST)));
        assertFalse(Boolean.parseBoolean(headers.get(REPLACE_HEADER_KEYS)));
        assertFalse(headers.toMultimap().containsKey(REMOVE_HEADER_KEYS));

        final JsonObject body = GsonUtils.getInstance().fromJson(Objects.requireNonNull(response.body()).string(), JsonObject.class);
        assertTrue(body.get(ADD_BODY_KEYS).getAsBoolean());
        assertNull(body.get(ORIGIN_REPLACE_BODY_KEYS));
        assertTrue(body.get(REPLACE_BODY_KEYS).getAsBoolean());
    }
}
