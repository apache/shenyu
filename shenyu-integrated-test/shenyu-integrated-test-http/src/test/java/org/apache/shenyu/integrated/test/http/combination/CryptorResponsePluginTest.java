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
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.AdminResponse;
import org.apache.shenyu.integratedtest.common.dto.UserDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.RsaStrategy;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.util.Base64;
import java.util.List;

import static org.apache.shenyu.integratedtest.common.utils.ConfUtils.ruleLocalData;
import static org.apache.shenyu.integratedtest.common.utils.ConfUtils.singletonRuleLocalDataList;
import static org.apache.shenyu.integratedtest.common.utils.ConfUtils.singletonURIEqConditionList;
import static org.apache.shenyu.plugin.api.result.ShenyuResultEnum.DECRYPTION_ERROR;
import static org.apache.shenyu.plugin.api.result.ShenyuResultEnum.ENCRYPTION_ERROR;
import static org.apache.shenyu.plugin.cryptor.strategy.MapTypeEnum.ALL;
import static org.apache.shenyu.plugin.cryptor.strategy.MapTypeEnum.FIELD;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class CryptorResponsePluginTest extends AbstractPluginDataInit {

    private static final String RSA_PRIVATE_KEY = "MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC3EAOwhNx5ofm5tcghJkelFTucfz2tXNvGSn2X27dCvc0BgKvHmQ598y2KKT+d0PqIJO6wqsmQaomqaSBPdXY8b+jvYUy/EYxA"
            + "aAjpf4vCtYy5KGIX9YEHVoSKsgjXXnUn0vq8HOvYnEBv3rrXYJiAHhmh1hb2bLnr1QjQOi8s/rwvb1VLiv5md3HHcLcQBJ92OWWA8fKxlYi/yIDa3iDtlqn/Gcc0iAQncwLnSeXuC274jdpMXpZX9KTO"
            + "HFlq3gYRd/MSsN+gW9nBWq1FPAbpIO13Fc+kwZZQQs50gJWv2Eniw0pshpyMS7BO91/3D2xGtO//9Sb7x+D25RLd73ExAgMBAAECggEADQvyY154sx+A44Qp3Pj0MrcCblsgK26aiDWPYWcKltJdnb"
            + "2MoJdPMdlGtdnOO6Jk9JaDP2qQnn8FTDSdVaRmtpR4OrVJybVHtGBlwDRzor8bJigTY6c+2KXJIPRizmygN2QhNA5wnZm3OvHaCZcMD1d11rOiI9JoZr8iV2rKKW/n+qyNckJTFNC88O5RFxmdMx2Q"
            + "6iGoSS3LGdXxCKhdKXdzGwD2cFDSIMMFCiO2SJDfzyj6mWVZipnduFuc2q9jD4nIcxeDXv1+wYHkUV301CJYvm/Cluw10+pJB193fuC15ZMMrOEuMtTWLAyG0BLSsU6831aqZHhra2RJw4Q2sQKBgQ"
            + "D+KkYhpep0vHDwvpq26cpvvj34f4BdevKPZxhu5INkQiVBffIfo/WiWgBMn6IrE+Mp2hf7HvvYmQNIDpW15fhutxnKhM4AnaGLNaWndSa/EiLNcATmVD9i+RowA+ZpELtWiG6pstn/DP5C17ttPhY5"
            + "wPCVxqN0amHUuBL+yo8JWQKBgQC4YlW2wsjRSsblyryYae7Orh2ZAa+2rvO/Vmkuf684tlEGlyg0ruF5i0kKYDkrl9ffLAmN+/3hzJ9ur5XrO/zGlsMsIyy+A+1cnitykUU93A2L//fSME4zR14PmT"
            + "PPyIYZxa1kLcEcDRCByuO2glk/44kfChzVygNbMdVKOWNTmQKBgQCm0QwyrXkSoVPnTtKw1wV9DfoSjWys7jMhl+LbdbQfK6LUN1uhFLX1lui3YdbIO0dPgstWkOFvKg6TTq9IMeY6lIai+0NR+CO9"
            + "ALr3C9cgdUDOYYV1vznTNffQJ98kekza4LTxQGgAFIEVUg68BpID2fSN+U/y6pfHTAF7pWr4EQKBgAYqw9MpEK5vYdetwEEYyfP/vt2vQMFLeLudmEcF3kZ3Up51z9JzRvdZwUenkEH1AjNkta0aEJ"
            + "PM1EhPdyQ3DW1W/ZAsXQK9/uJqJ+ndEgPPqGRWW2OcWgE9EdhTt3frrRCPnA0NurfFeBffQV6JXZLVeXCgVfaQmywhrpCc+sWBAoGAZIk5MEk+zrQt3CrH2UC/ly+1/DsrwAtYpWFlLR7KmFL9p+X8"
            + "rF6NdeaqiPNZ7KFLB+veOyriNRQ7oT3i8zmd2uv+DVokxlZ/QowBgUd2AGBudX5unAT0lEykf+4hK7mrvkePv+K2UsxPm+BfpIDI7ggq+4SWeDcx7OqGt6rU3Xc=";

    private static final String RSA_PUBLIC_KEY = "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAtxADsITceaH5ubXIISZHpRU7nH89rVzbxkp9l9u3Qr3NAYCrx5kOffMtiik/ndD6iCTusKrJkGqJqmkgT3V2PG/o72FMvxGMQGgI6X+Lw"
            + "rWMuShiF/WBB1aEirII1151J9L6vBzr2JxAb96612CYgB4ZodYW9my569UI0DovLP68L29VS4r+Zndxx3C3EASfdjllgPHysZWIv8iA2t4g7Zap/xnHNIgEJ3MC50nl7gtu+I3aTF6WV/SkzhxZat4G"
            + "EXfzErDfoFvZwVqtRTwG6SDtdxXPpMGWUELOdICVr9hJ4sNKbIacjEuwTvdf9w9sRrTv//Um+8fg9uUS3e9xMQIDAQAB";

    private static final String TEST_PATH = "/http/test/payment";

    private static final String TEST_USER_ID = "10001";

    private static final String TEST_USER_NAME = "user_name";

    private static final RsaStrategy RSA_STRATEGY = new RsaStrategy();

    private static final List<ConditionData> SINGLETON_CONDITION_LIST = singletonURIEqConditionList(TEST_PATH);

    private final UserDTO originalBody = new UserDTO(TEST_USER_ID, TEST_USER_NAME);

    @BeforeEach
    public void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.CRYPTOR_RESPONSE.getName(), null);
        assertThat(pluginResult, is("success"));
    }

    @Test
    @DisplayName("decrypt")
    public void testDecryptResponse() throws Exception {
        initSelectorAndRules(PluginEnum.CRYPTOR_RESPONSE.getName(),
                "",
                SINGLETON_CONDITION_LIST,
                buildRuleLocalDataList("userId", "decrypt", FIELD.getMapType()));

        JsonObject request = new JsonObject();
        request.addProperty("userId", RSA_STRATEGY.encrypt(RSA_PUBLIC_KEY, TEST_USER_ID));
        request.addProperty("userName", TEST_USER_NAME);
        String actualUserId = HttpHelper.INSTANCE.postGateway(TEST_PATH, request, String.class);
        assertThat(actualUserId, is(TEST_USER_ID));
    }

    @Test
    @DisplayName("encrypt")
    public void testEncryptResponse() throws Exception {
        initSelectorAndRules(PluginEnum.CRYPTOR_RESPONSE.getName(),
                "", SINGLETON_CONDITION_LIST, buildRuleLocalDataList("userName", "encrypt", ALL.getMapType()));

        JsonObject request = new JsonObject();
        request.addProperty("userId", TEST_USER_ID);
        request.addProperty("userName", TEST_USER_NAME);
        UserDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_PATH, request, UserDTO.class);
        byte[] inputByte = Base64.getMimeDecoder().decode(actualUser.getUserName());
        assertThat(RSA_STRATEGY.decrypt(RSA_PRIVATE_KEY, inputByte), is(TEST_USER_NAME));
    }

    @Test
    @DisplayName("skip this plugin when rule handle is null")
    public void testWhenRuleHandleIsNull() throws Exception {
        initSelectorAndRules(PluginEnum.CRYPTOR_RESPONSE.getName(),
                "", SINGLETON_CONDITION_LIST, singletonRuleLocalDataList(null, SINGLETON_CONDITION_LIST));

        UserDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_PATH, originalBody, UserDTO.class);

        assertThat(actualUser.getUserId(), is(originalBody.getUserId()));
        assertThat(actualUser.getUserName(), is(originalBody.getUserName()));

    }

    @Test
    @DisplayName("return original message when request doesnt exist filed")
    public void testWhenDoesntExistFiled() throws Exception {
        initSelectorAndRules(PluginEnum.CRYPTOR_RESPONSE.getName(),
                "", SINGLETON_CONDITION_LIST, buildRuleLocalDataList("data", "decrypt", FIELD.getMapType()));

        UserDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_PATH, originalBody, UserDTO.class);

        assertThat(actualUser.getUserId(), is(originalBody.getUserId()));
        assertThat(actualUser.getUserName(), is(originalBody.getUserName()));

    }

    @DisplayName("return failed message when decrypt or encrypt failed")
    @ParameterizedTest(name = "return failed message when {0} failed")
    @ValueSource(strings = {"decrypt", "encrypt"})
    public void testWhenDecryptionOrEncryptionIsFailed(final String way) throws Exception {

        CryptorRuleHandler handler = buildRuleHandler("rsa", 
                way, 
                "wrong_encrypt_key", 
                "wrong_decrypt_key", 
                "userId", 
                ALL.getMapType());
        RuleLocalData ruleLocalData = ruleLocalData(handler, SINGLETON_CONDITION_LIST);

        initSelectorAndRules(PluginEnum.CRYPTOR_RESPONSE.getName(), "", SINGLETON_CONDITION_LIST, Lists.newArrayList(ruleLocalData));

        AdminResponse response = HttpHelper.INSTANCE.postGateway(TEST_PATH, originalBody, AdminResponse.class);
        ShenyuResultEnum resultEnum = "decrypt".equals(way) ? DECRYPTION_ERROR : ENCRYPTION_ERROR;
        assertThat(response.getCode(), is(resultEnum.getCode()));
        assertThat(response.getMessage(), is(resultEnum.getMsg()));
    }

    @DisplayName("return failed message when key is null")
    @ParameterizedTest(name = "return failed message when {0}-key is null")
    @ValueSource(strings = {"decrypt", "encrypt"})
    public void testWhenKeyIsNull(final String way) throws Exception {

        CryptorRuleHandler handler = buildRuleHandler("rsa", 
                way, 
                null, 
                null, 
                "data", 
                ALL.getMapType());

        initSelectorAndRules(PluginEnum.CRYPTOR_RESPONSE.getName(),
                "", SINGLETON_CONDITION_LIST, singletonRuleLocalDataList(handler, SINGLETON_CONDITION_LIST));

        JsonObject request = new JsonObject();
        AdminResponse response = HttpHelper.INSTANCE.postGateway(TEST_PATH, request, AdminResponse.class);

        String keyName = "decrypt".equals(way) ? "decryptKey" : "encryptKey";
        assertThat(response.getMessage(), is(String.format("Please check Cryptor response plugin's [%s]", keyName)));
    }

    @DisplayName("return failed message when fieldNames is null")
    @Test
    public void testWhenFieldNamesIsNull() throws Exception {

        CryptorRuleHandler handler = buildRuleHandler("rsa", 
                "decrypt", 
                RSA_PUBLIC_KEY, 
                RSA_PRIVATE_KEY, 
                null, 
                ALL.getMapType());

        initSelectorAndRules(PluginEnum.CRYPTOR_RESPONSE.getName(), "", SINGLETON_CONDITION_LIST,
                singletonRuleLocalDataList(handler, SINGLETON_CONDITION_LIST));

        JsonObject request = new JsonObject();
        AdminResponse response = HttpHelper.INSTANCE.postGateway(TEST_PATH, request, AdminResponse.class);

        assertThat(response.getMessage(), is(String.format("Please check Cryptor response plugin's [%s]", "fieldNames")));
    }

    private List<RuleLocalData> buildRuleLocalDataList(final String fieldNames, final String way, final String mapType) {
        CryptorRuleHandler cryptorRuleHandler = buildRuleHandler("rsa", way, RSA_PUBLIC_KEY, RSA_PRIVATE_KEY, fieldNames, mapType);
        return singletonRuleLocalDataList(cryptorRuleHandler, SINGLETON_CONDITION_LIST);
    }

    private CryptorRuleHandler buildRuleHandler(final String strategyName, 
                                                final String way, 
                                                final String encryptKey, 
                                                final String decryptKey, 
                                                final String fieldNames,
                                                final String mapType) {
        CryptorRuleHandler cryptorRuleHandler = new CryptorRuleHandler();
        cryptorRuleHandler.setDecryptKey(decryptKey);
        cryptorRuleHandler.setEncryptKey(encryptKey);
        cryptorRuleHandler.setStrategyName(strategyName);
        cryptorRuleHandler.setFieldNames(fieldNames);
        cryptorRuleHandler.setWay(way);
        cryptorRuleHandler.setMapType(mapType);
        return cryptorRuleHandler;
    }

    @AfterEach
    public void clean() throws IOException {
        String cleanResult = cleanPluginData(PluginEnum.CRYPTOR_RESPONSE.getName());
        assertThat(cleanResult, is("success"));
    }
}
