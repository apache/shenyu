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

package org.apache.shenyu.integrated.test.combination;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.RuleHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingRuleHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.UserDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.RsaStrategy;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Base64;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * The integrated test for combination plugins about request.
 */
public final class MultiRequestPluginTest extends AbstractPluginDataInit {

    private static final String RSA_PRIVATE_KEY = "MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAvEXyUDh5qliWhM6KrpTFi1OXumoJQzMfSr8XjfKa/kHKb1uxr7N8lJd3I850m2IYrxckFCQW6nrnRKctm"
            + "iMgZQIDAQABAkBEFbdvMz0sUST9mgOk5sAZhn1UOIxo9M/YJArMlnNehqQs3Pv8RD6ASisgs19XnBhcUNdl2ecfxddp7OVQ6PVxAiEA+XagQdbwkFrEjUsPqPQTweKkc7aoVGJfifEGWvCKtAcCIQDBNN0K5vlVV5YKnA5WtDAN"
            + "K31oRGjTJe5D94IRYRHlMwIgQUH++jo4BAs6j5urJQ90e6vGSV7m+ewiAfvDJdb28dECIQCW9r9grWlVDcLnN1jc1p5VLA4pUoq1sYWjBdpTyg05kQIhAJUSfs1+gDFtCRekCwvn5QEnxJVJXD6SQPAPkEHS9Drb";

    private static final String RSA_PUBLIC_KEY = "MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALxF8lA4eapYloTOiq6UxYtTl7pqCUMzH0q/F43ymv5Bym9bsa+zfJSXdyPOdJtiGK8XJBQkFup650SnLZojIGUCAwEAAQ==";

    private static final String TEST_PATH = "/http/test/payment";

    private static final String TEST_USER_ID = "10001";

    private static final String TEST_USER_NAME = "user_name";

    private static final RsaStrategy RSA_STRATEGY = new RsaStrategy();

    private void setupParamMapping() throws IOException {
        String paramMappingRet = initPlugin(PluginEnum.PARAM_MAPPING.getName(), "{\"ruleHandlePageType\":\"custom\"}");
        assertThat(paramMappingRet, is("success"));
        String selectorAndRulesRet4ParamMapping = initSelectorAndRules(PluginEnum.PARAM_MAPPING.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList4ParamMapping());
        assertThat(selectorAndRulesRet4ParamMapping, is("success"));
    }

    private void setupCryptorRequest() throws IOException {
        String cryptorRequestRet = initPlugin(PluginEnum.CRYPTOR_REQUEST.getName(), null);
        assertThat(cryptorRequestRet, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.CRYPTOR_REQUEST.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList4CryptorRequest());
        assertThat(selectorAndRulesResult, is("success"));
    }

    /**
     * The combination test of cryptor request plugin and param mapping plugin.
     */
    @Test
    public void testCryptorRequestAndParamMapping() throws Exception {
        setupParamMapping();
        setupCryptorRequest();

        JsonObject request = new JsonObject();
        request.addProperty("userId", TEST_USER_ID);
        UserDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_PATH, request, UserDTO.class);
        byte[] inputByte = Base64.getMimeDecoder().decode(actualUser.getUserName());
        assertThat(RSA_STRATEGY.decrypt(RSA_PRIVATE_KEY, inputByte), is(TEST_USER_NAME));
        assertThat(actualUser.getUserId(), is(TEST_USER_ID));

        cleanParamMapping();
        cleanCryptorRequest();
    }

    private List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/**");
        return Collections.singletonList(conditionData);
    }

    private RuleLocalData buildRuleLocalData(final RuleHandle handle) {
        RuleLocalData ruleLocalData = new RuleLocalData();
        Optional.ofNullable(handle).ifPresent(ruleHandle -> ruleLocalData.setRuleHandler(JsonUtils.toJson(ruleHandle)));
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(TEST_PATH);
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return ruleLocalData;
    }

    private List<RuleLocalData> buildRuleLocalDataList4ParamMapping() {
        final List<RuleLocalData> ruleLocalDataList = new ArrayList<>();

        // prepare for addParameterKeys
        final ParamMappingRuleHandle addAndRemoveHandle = new ParamMappingRuleHandle();
        ParamMappingRuleHandle.ParamMapInfo addInfo = new ParamMappingRuleHandle.ParamMapInfo();
        addInfo.setPath("$");
        addInfo.setKey("userName");
        addInfo.setValue(TEST_USER_NAME);
        addAndRemoveHandle.setAddParameterKeys(Collections.singletonList(addInfo));
        ruleLocalDataList.add(buildRuleLocalData(addAndRemoveHandle));

        return ruleLocalDataList;
    }

    private List<RuleLocalData> buildRuleLocalDataList4CryptorRequest() {
        CryptorRuleHandler cryptorRuleHandler = new CryptorRuleHandler();
        cryptorRuleHandler.setDecryptKey(RSA_PRIVATE_KEY);
        cryptorRuleHandler.setEncryptKey(RSA_PUBLIC_KEY);
        cryptorRuleHandler.setStrategyName("rsa");
        cryptorRuleHandler.setFieldNames("userName");
        cryptorRuleHandler.setWay("encrypt");
        return Collections.singletonList(buildRuleLocalData(cryptorRuleHandler));
    }

    private void cleanParamMapping() throws IOException {
        String res = cleanPluginData(PluginEnum.PARAM_MAPPING.getName());
        assertThat(res, is("success"));
    }

    private void cleanCryptorRequest() throws IOException {
        String res = cleanPluginData(PluginEnum.CRYPTOR_REQUEST.getName());
        assertThat(res, is("success"));
    }
}
