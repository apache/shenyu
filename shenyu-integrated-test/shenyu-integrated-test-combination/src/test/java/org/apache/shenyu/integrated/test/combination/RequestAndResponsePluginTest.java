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
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.UserDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.RsaStrategy;
import org.apache.shenyu.web.controller.LocalPluginController;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * The integrated test for combination plugins about request and response.
 */
public final class RequestAndResponsePluginTest extends AbstractPluginDataInit {

    private static final String RSA_PRIVATE_KEY = "MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAvEXyUDh5qliWhM6KrpTFi1OXumoJQzMfSr8XjfKa/kHKb1uxr7N8lJd3I850m2IYrxckFCQW6nrnRKctm"
            + "iMgZQIDAQABAkBEFbdvMz0sUST9mgOk5sAZhn1UOIxo9M/YJArMlnNehqQs3Pv8RD6ASisgs19XnBhcUNdl2ecfxddp7OVQ6PVxAiEA+XagQdbwkFrEjUsPqPQTweKkc7aoVGJfifEGWvCKtAcCIQDBNN0K5vlVV5YKnA5WtDAN"
            + "K31oRGjTJe5D94IRYRHlMwIgQUH++jo4BAs6j5urJQ90e6vGSV7m+ewiAfvDJdb28dECIQCW9r9grWlVDcLnN1jc1p5VLA4pUoq1sYWjBdpTyg05kQIhAJUSfs1+gDFtCRekCwvn5QEnxJVJXD6SQPAPkEHS9Drb";

    private static final String RSA_PUBLIC_KEY = "MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALxF8lA4eapYloTOiq6UxYtTl7pqCUMzH0q/F43ymv5Bym9bsa+zfJSXdyPOdJtiGK8XJBQkFup650SnLZojIGUCAwEAAQ==";

    private static final String TEST_PATH = "/http/test/payment";

    private static final String TEST_USER_ID = "10001";

    private static final String TEST_USER_NAME = "user_name";

    private static final RsaStrategy RSA_STRATEGY = new RsaStrategy();

    @Test
    public void testDecryptRequestAndEncryptResponse() throws Exception {
        setupCryptorRequest("data", "decrypt");
        setupCryptorResponse("userName", "encrypt");

        JsonObject jsonObject = new JsonObject();
        jsonObject.addProperty("userId", TEST_USER_ID);
        jsonObject.addProperty("userName", TEST_USER_NAME);
        JsonObject request = new JsonObject();
        request.addProperty("data", RSA_STRATEGY.encrypt(RSA_PUBLIC_KEY, jsonObject.toString()));
        UserDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_PATH, request, UserDTO.class);
        byte[] inputByte = Base64.getMimeDecoder().decode(actualUser.getUserName());
        assertThat(RSA_STRATEGY.decrypt(RSA_PRIVATE_KEY, inputByte), is(TEST_USER_NAME));
        assertThat(actualUser.getUserId(), is(TEST_USER_ID));

        cleanCryptorRequest();
        cleanCryptorResponse();
    }

    @Test
    public void testEncryptRequestAndDecryptResponse() throws Exception {
        setupCryptorRequest("userName", "encrypt");
        setupCryptorResponse("userName", "decrypt");

        JsonObject request = new JsonObject();
        request.addProperty("userId", TEST_USER_ID);
        request.addProperty("userName", TEST_USER_NAME);
        String actualUserName = HttpHelper.INSTANCE.postGateway(TEST_PATH, request, String.class);
        assertThat(actualUserName, is(TEST_USER_NAME));

        cleanCryptorRequest();
        cleanCryptorResponse();
    }

    private List<LocalPluginController.RuleLocalData> buildRuleLocalDataList(final String fieldNames, final String way) {
        List<LocalPluginController.RuleLocalData> ruleLocalDataList = new ArrayList<>();
        ruleLocalDataList.add(buildRuleLocalData(fieldNames, way));
        return ruleLocalDataList;
    }

    private LocalPluginController.RuleLocalData buildRuleLocalData(final String fieldNames, final String way) {
        final LocalPluginController.RuleLocalData ruleLocalData = new LocalPluginController.RuleLocalData();

        CryptorRuleHandler cryptorRuleHandler = new CryptorRuleHandler();
        cryptorRuleHandler.setDecryptKey(RSA_PRIVATE_KEY);
        cryptorRuleHandler.setEncryptKey(RSA_PUBLIC_KEY);
        cryptorRuleHandler.setStrategyName("rsa");
        cryptorRuleHandler.setFieldNames(fieldNames);
        cryptorRuleHandler.setWay(way);

        ruleLocalData.setRuleHandler(JsonUtils.toJson(cryptorRuleHandler));
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(TEST_PATH);
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return ruleLocalData;
    }

    private List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(TEST_PATH);
        return Collections.singletonList(conditionData);
    }

    private void setupCryptorRequest(final String fieldNames, final String way) throws IOException {
        String requestPluginResult = initPlugin(PluginEnum.CRYPTOR_REQUEST.getName(), null);
        assertThat(requestPluginResult, is("success"));
        String initSelectorAndRules = initSelectorAndRules(PluginEnum.CRYPTOR_REQUEST.getName(),
                "", buildSelectorConditionList(), buildRuleLocalDataList(fieldNames, way));
        assertThat(initSelectorAndRules, is("success"));
    }

    private void setupCryptorResponse(final String fieldNames, final String way) throws IOException {
        String responsePluginResult = initPlugin(PluginEnum.CRYPTOR_RESPONSE.getName(), null);
        assertThat(responsePluginResult, is("success"));
        String cryptorResponseResult = initSelectorAndRules(PluginEnum.CRYPTOR_RESPONSE.getName(),
                "", buildSelectorConditionList(), buildRuleLocalDataList(fieldNames, way));
        assertThat(cryptorResponseResult, is("success"));
    }

    private void cleanCryptorRequest() throws IOException {
        String res = cleanPluginData(PluginEnum.CRYPTOR_REQUEST.getName());
        assertThat(res, is("success"));
    }

    private void cleanCryptorResponse() throws IOException {
        String res = cleanPluginData(PluginEnum.CRYPTOR_RESPONSE.getName());
        assertThat(res, is("success"));
    }
}
