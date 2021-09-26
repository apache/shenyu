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
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.OrderDTO;
import org.apache.shenyu.integratedtest.common.dto.UserDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

public class ParamMappingPluginTest extends AbstractPluginDataInit {

    @BeforeClass
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.PARAM_MAPPING.getName(), "{\"ruleHandlePageType\":\"custom\"}");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.PARAM_MAPPING.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
    }

    @Test
    public void test() throws IOException {
        // test addParameterKeys and removeParameterKeys
        Map<String, String> request1 = new HashMap<>();
        request1.put("userId", "123");
        UserDTO userDTO = HttpHelper.INSTANCE.postGateway("/http/test/payment", request1, UserDTO.class);
        assertNull(userDTO.getUserId());
        assertThat(userDTO.getUserName(), is("FixedName"));

        // test replaceParameterKeys
        Map<String, String> request2 = new HashMap<>();
        request2.put("i", "123");
        OrderDTO orderDTO = HttpHelper.INSTANCE.postGateway("/http/order/save", request2, OrderDTO.class);
        assertThat(orderDTO.getId(), is("123"));
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/**");
        return Collections.singletonList(conditionData);
    }

    private static List<LocalPluginController.RuleLocalData> buildRuleLocalDataList() {
        final List<LocalPluginController.RuleLocalData> ruleLocalDataList = new ArrayList<>();

        // prepare for addParameterKeys and removeParameterKeys
        final ParamMappingHandle addAndRemoveHandle = new ParamMappingHandle();
        ParamMappingHandle.ParamMapInfo addInfo = new ParamMappingHandle.ParamMapInfo();
        addInfo.setPath("$");
        addInfo.setKey("userName");
        addInfo.setValue("FixedName");
        addAndRemoveHandle.setAddParameterKeys(Collections.singletonList(addInfo));
        Set<String> removeSet = new HashSet<>();
        removeSet.add("$.userId");
        addAndRemoveHandle.setRemoveParameterKeys(removeSet);
        ruleLocalDataList.add(buildRuleLocalData(addAndRemoveHandle, "/http/test/payment"));

        // prepare for addParameterKeys and removeParameterKeys
        final ParamMappingHandle replaceHandle = new ParamMappingHandle();
        ParamMappingHandle.ParamMapInfo replaceInfo = new ParamMappingHandle.ParamMapInfo();
        replaceInfo.setPath("$");
        replaceInfo.setKey("i");
        replaceInfo.setValue("id");
        replaceHandle.setReplaceParameterKeys(Collections.singletonList(replaceInfo));
        ruleLocalDataList.add(buildRuleLocalData(replaceHandle, "/http/order/save"));
        return ruleLocalDataList;
    }

    private static LocalPluginController.RuleLocalData buildRuleLocalData(final ParamMappingHandle handle, final String paramValue) {
        LocalPluginController.RuleLocalData ruleLocalData = new LocalPluginController.RuleLocalData();
        ruleLocalData.setRuleHandler(JsonUtils.toJson(handle));
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(paramValue);
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return ruleLocalData;
    }

    @AfterClass
    public static void clean() throws IOException {
        cleanPluginData(PluginEnum.PARAM_MAPPING.getName());
    }
}
