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

package org.apache.shenyu.integrated.test.rewrite;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.RewriteHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.OrderDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * The integrated test for combination plugins about contextPath and rewrite.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ContextPathAndRewritePluginTest extends AbstractPluginDataInit {
    
    @Order(1)
    @Test
    public void testRewriteCrossApplication() throws IOException {
        OrderDTO orderDTO = HttpHelper.INSTANCE.getFromGateway("/http/order/findById?id=1", OrderDTO.class);
        assertEquals("1", orderDTO.getId());
        assertEquals("hello world findById", orderDTO.getName());
        
        orderDTO = HttpHelper.INSTANCE.getFromGateway("/order/order/findById?id=3", OrderDTO.class);
        assertNotNull(orderDTO);
        assertNull(orderDTO.getId());
        assertNull(orderDTO.getName());
        
        setupRewriteContextPath();
        
        OrderDTO result = HttpHelper.INSTANCE.getFromGateway("/order/order/findById?id=1", OrderDTO.class);
        assertEquals("1", result.getId());
        assertEquals("hello world findById", result.getName());

//        assertThat(cleanPluginData(PluginEnum.CONTEXT_PATH.getName()), is("success"));
    }
    
    @Order(2)
    @Test
    public void testRewriteCrossPlugin() throws IOException {
        OrderDTO orderDTO = HttpHelper.INSTANCE.getFromGateway("/http/order/findById?id=1", OrderDTO.class);
        assertEquals("1", orderDTO.getId());
        assertEquals("hello world findById", orderDTO.getName());
        
        setupRewrite();
        
        Map<String, Object> request = new HashMap<>();
        OrderDTO result = HttpHelper.INSTANCE.getFromGateway("/dubbo/findById?id=2", request, OrderDTO.class);
        assertEquals("2", result.getId());
        assertEquals("hello world findById", result.getName());
        
        assertThat(cleanPluginData(PluginEnum.CONTEXT_PATH.getName()), is("success"));
        assertThat(cleanPluginData(PluginEnum.REWRITE.getName()), is("success"));
    }
    
    private void setupRewrite() throws IOException {
        String pluginResult = initPlugin(PluginEnum.CONTEXT_PATH.getName(), "");
        assertThat(pluginResult, CoreMatchers.is("success"));
        LocalPluginController.RuleLocalData ruleLocalData = new LocalPluginController.RuleLocalData();
        ruleLocalData.setRuleHandler("{\"contextPath\":\"/dubbo\", \"addPrefix\":\"\", \"rewriteContextPath\":\"/http\", \"percentage\":100}");
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/dubbo/findById");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        String message = initSelectorAndRules(PluginEnum.CONTEXT_PATH.getName(), "", 0,
                buildSelectorConditionList("/dubbo/findById"), Collections.singletonList(ruleLocalData));
        assertThat(message, is("success"));
        
        pluginResult = initPlugin(PluginEnum.REWRITE.getName(), "");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.REWRITE.getName(), "", 0,
                buildSelectorConditionList("/http/findById"), buildRewriteRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
    }
    
    private static List<LocalPluginController.RuleLocalData> buildRewriteRuleLocalDataList() {
        final LocalPluginController.RuleLocalData ruleLocalData = new LocalPluginController.RuleLocalData();
        
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue("/http/findById");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        ruleLocalData.setRuleName("rewriteMetaData");
        
        RewriteHandle rewriteHandle = new RewriteHandle();
        rewriteHandle.setRegex("/http/findById");
        rewriteHandle.setReplace("/order/findById");
        rewriteHandle.setRewriteMetaData(true);
        rewriteHandle.setPercentage(100);
        
        ruleLocalData.setRuleHandler(JsonUtils.toJson(rewriteHandle));
        return Collections.singletonList(ruleLocalData);
    }
    
    private void setupRewriteContextPath() throws IOException {
        String pluginResult = initPlugin(PluginEnum.CONTEXT_PATH.getName(), "");
        assertThat(pluginResult, CoreMatchers.is("success"));
        LocalPluginController.RuleLocalData ruleLocalData = new LocalPluginController.RuleLocalData();
        ruleLocalData.setRuleHandler("{\"contextPath\":\"/order\", \"addPrefix\":\"\", \"rewriteContextPath\":\"/http\", \"percentage\":100}");
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/order/order/findById");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        String message = initSelectorAndRules(PluginEnum.CONTEXT_PATH.getName(), "",
                buildSelectorConditionList("/order/order/findById"), Collections.singletonList(ruleLocalData));
        assertThat(message, is("success"));
    }
    
    private List<ConditionData> buildSelectorConditionList(final String paramValue) {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(paramValue);
        return Collections.singletonList(conditionData);
    }
}
