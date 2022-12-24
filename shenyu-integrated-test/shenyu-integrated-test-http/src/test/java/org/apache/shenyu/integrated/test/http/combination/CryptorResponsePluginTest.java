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

import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.UserDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.RsaStrategy;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Base64;
import java.util.List;

import static org.apache.shenyu.integratedtest.common.util.ConfUtils.singletonRuleLocalDataList;
import static org.apache.shenyu.integratedtest.common.util.ConfUtils.singletonURIEqConditionList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class CryptorResponsePluginTest extends AbstractPluginDataInit {

    private static final String RSA_PRIVATE_KEY = "MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAvEXyUDh5qliWhM6KrpTFi1OXumoJQzMfSr8XjfKa/kHKb1uxr7N8lJd3I850m2IYrxckFCQW6nrnRKctm"
            + "iMgZQIDAQABAkBEFbdvMz0sUST9mgOk5sAZhn1UOIxo9M/YJArMlnNehqQs3Pv8RD6ASisgs19XnBhcUNdl2ecfxddp7OVQ6PVxAiEA+XagQdbwkFrEjUsPqPQTweKkc7aoVGJfifEGWvCKtAcCIQDBNN0K5vlVV5YKnA5WtDAN"
            + "K31oRGjTJe5D94IRYRHlMwIgQUH++jo4BAs6j5urJQ90e6vGSV7m+ewiAfvDJdb28dECIQCW9r9grWlVDcLnN1jc1p5VLA4pUoq1sYWjBdpTyg05kQIhAJUSfs1+gDFtCRekCwvn5QEnxJVJXD6SQPAPkEHS9Drb";

    private static final String RSA_PUBLIC_KEY = "MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALxF8lA4eapYloTOiq6UxYtTl7pqCUMzH0q/F43ymv5Bym9bsa+zfJSXdyPOdJtiGK8XJBQkFup650SnLZojIGUCAwEAAQ==";

    private static final String TEST_PATH = "/http/test/payment";

    private static final String TEST_USER_ID = "10001";

    private static final String TEST_USER_NAME = "user_name";

    private static final RsaStrategy RSA_STRATEGY = new RsaStrategy();

    private static final List<ConditionData> SINGLETON_CONDITION_LIST = singletonURIEqConditionList(TEST_PATH);

    @BeforeEach
    public void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.CRYPTOR_RESPONSE.getName(), null);
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void testDecryptResponse() throws Exception {
        initSelectorAndRules(PluginEnum.CRYPTOR_RESPONSE.getName(),
                "",
                singletonURIEqConditionList(TEST_PATH),
                buildRuleLocalDataList("userId", "decrypt"));

        JsonObject request = new JsonObject();
        request.addProperty("userId", RSA_STRATEGY.encrypt(RSA_PUBLIC_KEY, TEST_USER_ID));
        request.addProperty("userName", TEST_USER_NAME);
        String actualUserId = HttpHelper.INSTANCE.postGateway(TEST_PATH, request, String.class);
        assertThat(actualUserId, is(TEST_USER_ID));
    }

    @Test
    public void testEncryptResponse() throws Exception {
        initSelectorAndRules(PluginEnum.CRYPTOR_RESPONSE.getName(),
                "", singletonURIEqConditionList(TEST_PATH), buildRuleLocalDataList("userName", "encrypt"));

        JsonObject request = new JsonObject();
        request.addProperty("userId", TEST_USER_ID);
        request.addProperty("userName", TEST_USER_NAME);
        UserDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_PATH, request, UserDTO.class);
        byte[] inputByte = Base64.getMimeDecoder().decode(actualUser.getUserName());
        assertThat(RSA_STRATEGY.decrypt(RSA_PRIVATE_KEY, inputByte), is(TEST_USER_NAME));
    }

    private List<RuleLocalData> buildRuleLocalDataList(final String fieldNames, final String way) {
        CryptorRuleHandler cryptorRuleHandler = buildRuleHandler("rsa", way, RSA_PUBLIC_KEY, RSA_PRIVATE_KEY, fieldNames);
        return singletonRuleLocalDataList(cryptorRuleHandler, SINGLETON_CONDITION_LIST);
    }

    private CryptorRuleHandler buildRuleHandler(final String strategyName, final String way, final String encryptKey, final String decryptKey, final String fieldNames) {
        CryptorRuleHandler cryptorRuleHandler = new CryptorRuleHandler();
        cryptorRuleHandler.setDecryptKey(decryptKey);
        cryptorRuleHandler.setEncryptKey(encryptKey);
        cryptorRuleHandler.setStrategyName(strategyName);
        cryptorRuleHandler.setFieldNames(fieldNames);
        cryptorRuleHandler.setWay(way);
        return cryptorRuleHandler;
    }

    @AfterEach
    public void clean() throws IOException {
        String cleanResult = cleanPluginData(PluginEnum.CRYPTOR_RESPONSE.getName());
        assertThat(cleanResult, is("success"));
    }
}
