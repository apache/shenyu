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

import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.RateLimiterHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingRuleHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.DubboTest;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.RsaStrategy;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Base64;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * The integrated test for combination plugins about request.
 */
public final class RpcAndRequestPluginTest extends AbstractPluginDataInit {

    private static final String RSA_PRIVATE_KEY = "MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAvEXyUDh5qliWhM6KrpTFi1OXumoJQzMfSr8XjfKa/kHKb1uxr7N8lJd3I850m2IYrxckFCQW6nrnRKctm"
            + "iMgZQIDAQABAkBEFbdvMz0sUST9mgOk5sAZhn1UOIxo9M/YJArMlnNehqQs3Pv8RD6ASisgs19XnBhcUNdl2ecfxddp7OVQ6PVxAiEA+XagQdbwkFrEjUsPqPQTweKkc7aoVGJfifEGWvCKtAcCIQDBNN0K5vlVV5YKnA5WtDAN"
            + "K31oRGjTJe5D94IRYRHlMwIgQUH++jo4BAs6j5urJQ90e6vGSV7m+ewiAfvDJdb28dECIQCW9r9grWlVDcLnN1jc1p5VLA4pUoq1sYWjBdpTyg05kQIhAJUSfs1+gDFtCRekCwvn5QEnxJVJXD6SQPAPkEHS9Drb";

    private static final String RSA_PUBLIC_KEY = "MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALxF8lA4eapYloTOiq6UxYtTl7pqCUMzH0q/F43ymv5Bym9bsa+zfJSXdyPOdJtiGK8XJBQkFup650SnLZojIGUCAwEAAQ==";

    private static final String TEST_PATH = "/dubbo/findById";

    private static final String TEST_ID = "10001";

    private static final RsaStrategy RSA_STRATEGY = new RsaStrategy();

    private static final Map<String, Object> DUBBO_REQUEST = new HashMap<>();

    @BeforeAll
    public static void setup() throws IOException {
        String dubboPluginResult = initPlugin(PluginEnum.DUBBO.getName(), "{\"register\":\"zookeeper://shenyu-zk:2181\"}");
        assertThat(dubboPluginResult, is("success"));
        DUBBO_REQUEST.put("id", TEST_ID);
    }

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

    private void setupRateLimiter() throws IOException {
        String pluginResult = initPlugin(PluginEnum.RATE_LIMITER.getName(), "{\"mode\":\"standalone\",\"master\":\"mymaster\",\"url\":\"shenyu-redis:6379\",\"password\":\"abc\"}");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.RATE_LIMITER.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList4RateLimiter());
        assertThat(selectorAndRulesResult, is("success"));
    }

    /**
     * The combination test of Dubbo plugin and ParamMapping plugin.
     */
    @Test
    public void testDubboAndParamMapping() throws IOException {
        setupParamMapping();

        Map<String, Object> request = new HashMap<>();
        DubboTest result = HttpHelper.INSTANCE.postGateway(TEST_PATH, request, new TypeToken<DubboTest>() { }.getType());
        assertEquals(TEST_ID, result.getId());

        cleanParamMapping();
    }

    /**
     * The combination test of Dubbo plugin and CryptorRequest plugin.
     */
    @Test
    public void testDubboAndCryptorRequest() throws Exception {
        setupCryptorRequest();

        DubboTest result = HttpHelper.INSTANCE.postGateway(TEST_PATH, DUBBO_REQUEST, new TypeToken<DubboTest>() { }.getType());
        byte[] inputByte = Base64.getMimeDecoder().decode(result.getId());
        assertEquals(TEST_ID, RSA_STRATEGY.decrypt(RSA_PRIVATE_KEY, inputByte));

        cleanCryptorRequest();
    }

    /**
     * The combination test of Dubbo plugin and RateLimiter plugin.
     */
    @Test
    public void testDubboAndRateLimiter() throws IOException, ExecutionException, InterruptedException {
        setupRateLimiter();

        Future<DubboTest> allowedRespFuture1 = this.getService().submit(() ->
                HttpHelper.INSTANCE.postGateway(TEST_PATH, DUBBO_REQUEST, new TypeToken<DubboTest>() { }.getType()));
        assertEquals(TEST_ID, allowedRespFuture1.get().getId());

        Thread.sleep(5000);
        Future<DubboTest> allowedRespFuture2 = this.getService().submit(() ->
                HttpHelper.INSTANCE.postGateway(TEST_PATH, DUBBO_REQUEST, new TypeToken<DubboTest>() { }.getType()));
        assertEquals(TEST_ID, allowedRespFuture2.get().getId());

        cleanRateLimiter();
    }

    private List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/dubbo/**");
        return Collections.singletonList(conditionData);
    }

    private RuleLocalData buildRuleLocalData() {
        final RuleLocalData ruleLocalData = new RuleLocalData();
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(TEST_PATH);
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return ruleLocalData;
    }

    private List<RuleLocalData> buildRuleLocalDataList4ParamMapping() {
        // prepare for addParameterKeys
        final ParamMappingRuleHandle addAndRemoveHandle = new ParamMappingRuleHandle();
        ParamMappingRuleHandle.ParamMapInfo addInfo = new ParamMappingRuleHandle.ParamMapInfo();
        addInfo.setPath("$");
        addInfo.setKey("id");
        addInfo.setValue(TEST_ID);
        addAndRemoveHandle.setAddParameterKeys(Collections.singletonList(addInfo));

        RuleLocalData ruleLocalData = buildRuleLocalData();
        ruleLocalData.setRuleHandler(JsonUtils.toJson(addAndRemoveHandle));
        return Collections.singletonList(ruleLocalData);
    }

    private List<RuleLocalData> buildRuleLocalDataList4CryptorRequest() {
        CryptorRuleHandler cryptorRuleHandler = new CryptorRuleHandler();
        cryptorRuleHandler.setDecryptKey(RSA_PRIVATE_KEY);
        cryptorRuleHandler.setEncryptKey(RSA_PUBLIC_KEY);
        cryptorRuleHandler.setStrategyName("rsa");
        cryptorRuleHandler.setFieldNames("id");
        cryptorRuleHandler.setWay("encrypt");

        RuleLocalData ruleLocalData = buildRuleLocalData();
        ruleLocalData.setRuleHandler(JsonUtils.toJson(cryptorRuleHandler));
        return Collections.singletonList(ruleLocalData);
    }

    private List<RuleLocalData> buildRuleLocalDataList4RateLimiter() {
        RateLimiterHandle rateLimiterHandle = new RateLimiterHandle();
        rateLimiterHandle.setAlgorithmName("tokenBucket");
        rateLimiterHandle.setReplenishRate(0.2);
        rateLimiterHandle.setBurstCapacity(1);
        // see WholeKeyResolver.java
        rateLimiterHandle.setKeyResolverName("WHOLE_KEY_RESOLVER");

        RuleLocalData ruleLocalData = buildRuleLocalData();
        ruleLocalData.setRuleHandler(JsonUtils.toJson(rateLimiterHandle));

        return Collections.singletonList(ruleLocalData);
    }

    private void cleanParamMapping() throws IOException {
        String res = cleanPluginData(PluginEnum.PARAM_MAPPING.getName());
        assertThat(res, is("success"));
    }

    private void cleanCryptorRequest() throws IOException {
        String res = cleanPluginData(PluginEnum.CRYPTOR_REQUEST.getName());
        assertThat(res, is("success"));
    }

    private void cleanRateLimiter() throws IOException {
        String res = cleanPluginData(PluginEnum.RATE_LIMITER.getName());
        assertThat(res, is("success"));
    }
}
