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

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.jsonwebtoken.security.Keys;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.convert.rule.RateLimiterHandle;
import org.apache.shenyu.common.dto.convert.rule.RedirectHandle;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle;
import org.apache.shenyu.common.dto.convert.rule.RewriteHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingRuleHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.AdminResponse;
import org.apache.shenyu.integratedtest.common.dto.ModifyResponseDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.RsaStrategy;
import org.apache.shenyu.web.controller.LocalPluginController;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.Test;

import com.google.common.reflect.TypeToken;
import com.google.gson.JsonObject;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;

/**
 * The integrated test for combination plugins about request.
 */
public final class MultiRequestPluginTest extends AbstractPluginDataInit {

    private static final String RSA_PRIVATE_KEY =
            "MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAvEXyUDh5qliWhM6KrpTFi1OXumoJQzMfSr8XjfKa/kHKb1uxr7N8lJd3I850m2IYrxckFCQW6nrnRKctm"
                    + "iMgZQIDAQABAkBEFbdvMz0sUST9mgOk5sAZhn1UOIxo9M/YJArMlnNehqQs3Pv8RD6ASisgs19XnBhcUNdl2ecfxddp7OVQ6PVxAiEA"
                    + "+XagQdbwkFrEjUsPqPQTweKkc7aoVGJfifEGWvCKtAcCIQDBNN0K5vlVV5YKnA5WtDAN"
                    + "K31oRGjTJe5D94IRYRHlMwIgQUH++jo4BAs6j5urJQ90e6vGSV7m+ewiAfvDJdb28dECIQCW9r9grWlVDcLnN1jc1p5VLA4pUoq1sYWjBdpTyg05kQIhAJUSfs1"
                    + "+gDFtCRekCwvn5QEnxJVJXD6SQPAPkEHS9Drb";

    private static final String RSA_PUBLIC_KEY =
            "MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALxF8lA4eapYloTOiq6UxYtTl7pqCUMzH0q/F43ymv5Bym9bsa+zfJSXdyPOdJtiGK8XJBQkFup650SnLZojIGUCAwEAAQ==";

    private static final String TEST_MODIFY_REQUEST = "/http/test/modifyRequest";

    private static final String TEST_REDIRECT_URL = "/shenyu/plugin/saveOrUpdate";

    private static final String TEST_USER_ID = "10001";

    private static final String TEST_USER_NAME = "user_name";

    private static final String AFTER_REWRITING_PATH = "/test/modifyRequest";

    private static final RsaStrategy RSA_STRATEGY = new RsaStrategy();

    private void setupParamMapping() throws IOException {
        String paramMappingRet = initPlugin(PluginEnum.PARAM_MAPPING.getName(), "{\"ruleHandlePageType\":\"custom\"}");
        assertThat(paramMappingRet, is("success"));
        String selectorAndRulesRet4ParamMapping =
                initSelectorAndRules(PluginEnum.PARAM_MAPPING.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList4ParamMapping());
        assertThat(selectorAndRulesRet4ParamMapping, is("success"));
    }

    private void setupCryptorRequest() throws IOException {
        String cryptorRequestRet = initPlugin(PluginEnum.CRYPTOR_REQUEST.getName(), null);
        assertThat(cryptorRequestRet, is("success"));
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.CRYPTOR_REQUEST.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList4CryptorRequest());
        assertThat(selectorAndRulesResult, is("success"));
    }

    private void setupRewrite() throws IOException {
        String paramMappingRet = initPlugin(PluginEnum.REWRITE.getName(), "{\"ruleHandlePageType\":\"custom\"}");
        assertThat(paramMappingRet, is("success"));
        String selectorAndRulesRet4ParamMapping =
                initSelectorAndRules(PluginEnum.REWRITE.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList4Rewrite());
        assertThat(selectorAndRulesRet4ParamMapping, is("success"));
    }

    private void setupRedirect() throws IOException {
        String paramMappingRet = initPlugin(PluginEnum.REDIRECT.getName(), "{\"ruleHandlePageType\":\"custom\"}");
        assertThat(paramMappingRet, is("success"));
        String selectorAndRulesRet4ParamMapping =
                initSelectorAndRules(PluginEnum.REDIRECT.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList4Redirect());
        assertThat(selectorAndRulesRet4ParamMapping, is("success"));
    }

    private void setupRateLimiter(final String algorithmName) throws IOException {
        String paramMappingRet = initPlugin(PluginEnum.RATE_LIMITER.getName(),
                "{\"mode\":\"standalone\",\"master\":\"mymaster\",\"url\":\"shenyu-redis:6379\",\"password\":\"abc\"}");
        assertThat(paramMappingRet, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.RATE_LIMITER.getName(), "", buildSelectorConditionList(),
                buildRuleLocalDataList4RateLimiter(algorithmName));
        assertThat(selectorAndRulesResult, is("success"));
    }

    private void setupRequest() throws IOException {
        String paramMappingRet = initPlugin(PluginEnum.REQUEST.getName(), null);
        assertThat(paramMappingRet, is("success"));
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.REQUEST.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList4Request());
        assertThat(selectorAndRulesResult, is("success"));
    }

    private void setupJWT() throws IOException {
        // HMAC-SHA algorithms MUST have a size >= 256 bits
        String pluginResult = initPlugin(PluginEnum.JWT.getName(), "{\"secretKey\":\"shenyu-test-shenyu-test-shenyu-test\"}");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult =
                initSelectorAndRules(PluginEnum.JWT.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList4JWT());
        assertThat(selectorAndRulesResult, is("success"));
    }

    /**
     * The combination test of cryptor request plugin and param mapping plugin.
     */
    @Test
    public void testCryptorRequestAndParamMapping() throws Exception {
        try {
            setupParamMapping();
            setupCryptorRequest();

            JsonObject request = new JsonObject();
            request.addProperty("userId", TEST_USER_ID);
            ModifyResponseDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, request, ModifyResponseDTO.class);
            byte[] inputByte = Base64.getMimeDecoder().decode(actualUser.getBody().getUserName());
            assertThat(RSA_STRATEGY.decrypt(RSA_PRIVATE_KEY, inputByte), is(TEST_USER_NAME));
            assertThat(actualUser.getBody().getUserId(), is(TEST_USER_ID));
        } finally {
            cleanParamMapping();
            cleanCryptorRequest();
        }
    }

    /**
     * The combination test of Rewrite plugin and param mapping plugin.
     */
    @Test
    public void testRewriteAndParamMapping() throws Exception {
        try {
            setupParamMapping();
            setupRewrite();

            JsonObject request = new JsonObject();
            request.addProperty("userId", TEST_USER_ID);
            ModifyResponseDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, request, ModifyResponseDTO.class);
            assertThat(actualUser.getBody().getUserName(), is(TEST_USER_NAME));
            assertThat(actualUser.getBody().getUserId(), is(TEST_USER_ID));
        } finally {
            cleanParamMapping();
            cleanRewrite();
        }
    }

    /**
     * The combination test of Redirect plugin and param mapping plugin.
     */
    @Test
    public void testRedirectAndParamMapping() throws Exception {
        try {
            setupParamMapping();
            setupRedirect();

            PluginData pluginData = new PluginData();
            pluginData.setEnabled(true);
            pluginData.setName(PluginEnum.REDIRECT.getName());
            pluginData.setConfig("{\"ruleHandlePageType\":\"custom\"}");

            String response = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, pluginData, String.class);
            assertThat(response, is("success"));
        } finally {
            cleanParamMapping();
            cleanRedirect();
        }
    }

    /**
     * The combination test of Rewrite plugin and RateLimiter plugin with slidingWindow.
     */
    @Test
    public void testSlidingWindowAndRewrite() throws IOException, InterruptedException {
        testRateLimiterAndRewrite("slidingWindow");
    }

    /**
     * The combination test of Rewrite plugin and RateLimiter plugin with leakyBucket.
     */
    @Test
    public void testLeakyBucketAndRewrite() throws IOException, InterruptedException {
        testRateLimiterAndRewrite("leakyBucket");
    }

    /**
     * The combination test of Rewrite plugin and RateLimiter plugin with tokenBucket.
     */
    @Test
    public void testTokenBucketAndRewrite() throws IOException, InterruptedException {
        testRateLimiterAndRewrite("tokenBucket");
    }

    /**
     * The combination test of Rewrite plugin and RateLimiter plugin with leakyBucket.
     */
    @Test
    public void testLeakyBucketAndRedirect() throws IOException, InterruptedException {
        testRateLimiterAndRedirect("leakyBucket");
    }

    /**
     * The combination test of Rewrite plugin and RateLimiter plugin with tokenBucket.
     */
    @Test
    public void testTokenBucketAndRedirect() throws IOException, InterruptedException {
        testRateLimiterAndRedirect("tokenBucket");
    }

    /**
     * The combination test of Rewrite plugin and RateLimiter plugin with slidingWindow.
     */
    @Test
    public void testSlidingWindowAndRedirect() throws IOException, InterruptedException {
        testRateLimiterAndRedirect("slidingWindow");
    }

    /**
     * The combination test of Request plugin and Rewrite plugin.
     */
    @Test
    public void testRequestAndRewrite() throws IOException {
        try {
            setupRequest();
            setupRewrite();

            JsonObject request = new JsonObject();
            request.addProperty("userId", TEST_USER_ID);
            request.addProperty("userName", TEST_USER_NAME);
            ModifyResponseDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, request, ModifyResponseDTO.class);
            assertThat(actualUser.getBody().getUserName(), is(TEST_USER_NAME));
            assertThat(actualUser.getBody().getUserId(), is(TEST_USER_ID));

            assertThat(actualUser.getCookie(), is("cookie"));
            assertThat(actualUser.getHeader(), is("requestHeader"));
            assertThat(actualUser.getParameter(), is("requestParameter"));
        } finally {
            cleanRequest();
            cleanRewrite();
        }
    }

    /**
     * The combination test of Request plugin and Redirect plugin.
     */
    @Test
    public void testRequestAndRedirect() throws IOException {
        try {
            setupRequest();
            setupRedirect();

            PluginData pluginData = new PluginData();
            pluginData.setEnabled(true);
            pluginData.setName(PluginEnum.REDIRECT.getName());
            pluginData.setConfig("{\"ruleHandlePageType\":\"custom\"}");

            String response = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, pluginData, String.class);
            assertThat(response, is("success"));
        } finally {
            cleanRequest();
            cleanRedirect();
        }
    }

    /**
     * The combination test of JWT plugin and Redirect plugin.
     */
    @Test
    public void testJWTAndRedirect() throws IOException {
        try {
            setupJWT();
            setupRedirect();

            final String key = "shenyu-test-shenyu-test-shenyu-test";
            final String token = Jwts.builder().setId("1001").signWith(Keys.hmacShaKeyFor(key.getBytes(StandardCharsets.UTF_8)), SignatureAlgorithm.HS256).compact();
            Map<String, Object> headers = new HashMap<>();
            headers.put("token", token);
            PluginData pluginData = new PluginData();
            pluginData.setEnabled(true);
            pluginData.setName(PluginEnum.REDIRECT.getName());
            pluginData.setConfig("{\"ruleHandlePageType\":\"custom\"}");

            String response = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, headers, pluginData, String.class);
            assertThat(response, is("success"));
        } finally {
            setupJWT();
            cleanRedirect();
        }
    }

    /**
     * The combination test of JWT plugin and RateLimiter plugin with tokenBucket.
     */
    @Test
    public void testJWTAndTokenBucket() throws IOException, InterruptedException {
        testRateLimiterAndJWT("tokenBucket");
    }

    /**
     * The combination test of JWT plugin and RateLimiter plugin with slidingWindow.
     */
    @Test
    public void testJWTAndSlidingWindow() throws IOException, InterruptedException {
        testRateLimiterAndJWT("slidingWindow");
    }

    /**
     * The combination test of JWT plugin and RateLimiter plugin with leakyBucket.
     */
    @Test
    public void testJWTAndLeakyBucket() throws IOException, InterruptedException {
        testRateLimiterAndJWT("leakyBucket");
    }

    /**
     * The combination test of JWT plugin and Rewrite plugin.
     */
    @Test
    public void testJWTAndRewrite() throws IOException {
        try {
            setupJWT();
            setupRewrite();

            final String key = "shenyu-test-shenyu-test-shenyu-test";
            final String token = Jwts.builder().setId("1001").signWith(Keys.hmacShaKeyFor(key.getBytes(StandardCharsets.UTF_8)), SignatureAlgorithm.HS256).compact();
            Map<String, Object> headers = new HashMap<>();
            headers.put("token", token);

            JsonObject request = new JsonObject();
            request.addProperty("userId", TEST_USER_ID);
            request.addProperty("userName", TEST_USER_NAME);
            ModifyResponseDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, headers, request, ModifyResponseDTO.class);

            assertThat(actualUser.getBody().getUserName(), is(TEST_USER_NAME));
            assertThat(actualUser.getBody().getUserId(), is(TEST_USER_ID));
        } finally {
            cleanJWT();
            cleanRewrite();
        }
    }

    private void testRateLimiterAndRedirect(final String algorithmName) throws IOException, InterruptedException {
        try {
            setupRateLimiter(algorithmName);
            setupRedirect();

            PluginData pluginData = new PluginData();
            pluginData.setEnabled(true);
            pluginData.setName(PluginEnum.REDIRECT.getName());
            pluginData.setConfig("{\"ruleHandlePageType\":\"custom\"}");

            String response = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, pluginData, String.class);
            assertThat(response, is("success"));

            AdminResponse<Object> errorRsp = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, pluginData, new TypeToken<AdminResponse<Object>>() {
            }.getType());
            assertThat(errorRsp.getMessage(), is("You have been restricted, please try again later!"));
            assertThat(errorRsp.getCode(), is(429));

            Thread.sleep(2000);

            response = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, pluginData, String.class);
            assertThat(response, is("success"));
        } finally {
            cleanRateLimiter();
            cleanRedirect();
        }
    }

    private void testRateLimiterAndRewrite(final String algorithmName) throws IOException, InterruptedException {
        try {
            setupRateLimiter(algorithmName);
            setupRewrite();

            JsonObject request = new JsonObject();
            request.addProperty("userId", TEST_USER_ID);
            request.addProperty("userName", TEST_USER_NAME);
            ModifyResponseDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, request, ModifyResponseDTO.class);
            assertThat(actualUser.getBody().getUserName(), is(TEST_USER_NAME));
            assertThat(actualUser.getBody().getUserId(), is(TEST_USER_ID));

            AdminResponse<Object> errorRsp = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, request, new TypeToken<AdminResponse<Object>>() {
            }.getType());
            assertThat(errorRsp.getMessage(), is("You have been restricted, please try again later!"));
            assertThat(errorRsp.getCode(), is(429));

            Thread.sleep(2000);

            actualUser = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, request, ModifyResponseDTO.class);
            assertThat(actualUser.getBody().getUserName(), is(TEST_USER_NAME));
            assertThat(actualUser.getBody().getUserId(), is(TEST_USER_ID));
        } finally {
            cleanRateLimiter();
            cleanRewrite();
        }
    }

    private void testRateLimiterAndJWT(final String algorithmName) throws IOException, InterruptedException {
        try {
            setupRateLimiter(algorithmName);
            setupJWT();

            final String key = "shenyu-test-shenyu-test-shenyu-test";
            final String token = Jwts.builder().setId("1001").signWith(Keys.hmacShaKeyFor(key.getBytes(StandardCharsets.UTF_8)), SignatureAlgorithm.HS256).compact();
            Map<String, Object> headers = new HashMap<>();
            headers.put("token", token);

            JsonObject request = new JsonObject();
            request.addProperty("userId", TEST_USER_ID);
            request.addProperty("userName", TEST_USER_NAME);
            ModifyResponseDTO actualUser = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, headers, request, ModifyResponseDTO.class);
            assertThat(actualUser.getBody().getUserName(), is(TEST_USER_NAME));
            assertThat(actualUser.getBody().getUserId(), is(TEST_USER_ID));

            AdminResponse<Object> errorRsp =
                    HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, headers, request, new TypeToken<AdminResponse<Object>>() {
                    }.getType());
            assertThat(errorRsp.getMessage(), is("You have been restricted, please try again later!"));
            assertThat(errorRsp.getCode(), is(429));

            Thread.sleep(2000);

            actualUser = HttpHelper.INSTANCE.postGateway(TEST_MODIFY_REQUEST, headers, request, ModifyResponseDTO.class);
            assertThat(actualUser.getBody().getUserName(), is(TEST_USER_NAME));
            assertThat(actualUser.getBody().getUserId(), is(TEST_USER_ID));
        } finally {
            cleanRateLimiter();
            cleanJWT();
        }
    }

    private List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/**");
        return Collections.singletonList(conditionData);
    }

    private RuleLocalData buildRuleLocalData(final String handleStr) {
        RuleLocalData ruleLocalData = new RuleLocalData();
        ruleLocalData.setRuleHandler(handleStr);
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(MultiRequestPluginTest.TEST_MODIFY_REQUEST);
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return ruleLocalData;
    }

    private List<RuleLocalData> buildRuleLocalDataList4ParamMapping() {
        final List<RuleLocalData> ruleLocalDataList = new ArrayList<>();

        final ParamMappingRuleHandle addAndRemoveHandle = new ParamMappingRuleHandle();
        // prepare for addParameterKeys
        ParamMappingRuleHandle.ParamMapInfo addInfo = new ParamMappingRuleHandle.ParamMapInfo();
        addInfo.setPath("$");
        addInfo.setKey("userName");
        addInfo.setValue(TEST_USER_NAME);
        addAndRemoveHandle.setAddParameterKeys(Collections.singletonList(addInfo));

        ruleLocalDataList.add(buildRuleLocalData(JsonUtils.toJson(addAndRemoveHandle)));

        return ruleLocalDataList;
    }

    private List<RuleLocalData> buildRuleLocalDataList4CryptorRequest() {
        CryptorRuleHandler cryptorRuleHandler = new CryptorRuleHandler();
        cryptorRuleHandler.setDecryptKey(RSA_PRIVATE_KEY);
        cryptorRuleHandler.setEncryptKey(RSA_PUBLIC_KEY);
        cryptorRuleHandler.setStrategyName("rsa");
        cryptorRuleHandler.setFieldNames("userName");
        cryptorRuleHandler.setWay("encrypt");
        cryptorRuleHandler.setMapType("all");
        return Collections.singletonList(buildRuleLocalData(JsonUtils.toJson(cryptorRuleHandler)));
    }

    private List<LocalPluginController.RuleLocalData> buildRuleLocalDataList4Rewrite() {
        final List<LocalPluginController.RuleLocalData> ruleLocalDataList = new ArrayList<>();
        RewriteHandle rewriteHandle = new RewriteHandle();
        rewriteHandle.setRegex(TEST_MODIFY_REQUEST);
        rewriteHandle.setReplace(AFTER_REWRITING_PATH);

        ruleLocalDataList.add(buildRuleLocalData(JsonUtils.toJson(rewriteHandle)));

        return ruleLocalDataList;
    }

    private List<RuleLocalData> buildRuleLocalDataList4Redirect() {
        final List<RuleLocalData> ruleLocalDataList = new ArrayList<>();
        RedirectHandle redirectHandle = new RedirectHandle();
        redirectHandle.setRedirectURI(TEST_REDIRECT_URL);

        ruleLocalDataList.add(buildRuleLocalData(JsonUtils.toJson(redirectHandle)));

        return ruleLocalDataList;
    }

    private List<RuleLocalData> buildRuleLocalDataList4RateLimiter(final String algorithmName) {
        RateLimiterHandle rateLimiterHandle = new RateLimiterHandle();
        rateLimiterHandle.setAlgorithmName(algorithmName);
        rateLimiterHandle.setReplenishRate(0.5);
        rateLimiterHandle.setBurstCapacity(1);
        // see WholeKeyResolver.java
        rateLimiterHandle.setKeyResolverName("WHOLE_KEY_RESOLVER");
        return Collections.singletonList(buildRuleLocalData(JsonUtils.toJson(rateLimiterHandle)));
    }

    private List<RuleLocalData> buildRuleLocalDataList4Request() {
        RequestHandle requestHandle = new RequestHandle();
        final RequestHandle.ShenyuRequestParameter requestParameter = requestHandle.new ShenyuRequestParameter();
        final RequestHandle.ShenyuRequestHeader requestHeader = requestHandle.new ShenyuRequestHeader();
        final RequestHandle.ShenyuCookie cookie = requestHandle.new ShenyuCookie();

        Map<String, String> paramMap = new HashMap<>();
        paramMap.put("requestParameter", "requestParameter");
        requestParameter.setSetParameters(new HashMap<>(paramMap));
        requestHandle.setParameter(requestParameter);

        paramMap.clear();
        paramMap.put("requestHeader", "requestHeader");
        requestHeader.setSetHeaders(new HashMap<>(paramMap));
        requestHandle.setHeader(requestHeader);

        paramMap.clear();
        paramMap.put("cookie", "cookie");
        cookie.setSetCookies(new HashMap<>(paramMap));
        requestHandle.setCookie(cookie);

        return Collections.singletonList(buildRuleLocalData(JsonUtils.toJson(requestHandle)));
    }

    private List<RuleLocalData> buildRuleLocalDataList4JWT() {
        return Collections.singletonList(buildRuleLocalData(null));
    }

    private void cleanParamMapping() throws IOException {
        String res = cleanPluginData(PluginEnum.PARAM_MAPPING.getName());
        assertThat(res, is("success"));
    }

    private void cleanCryptorRequest() throws IOException {
        String res = cleanPluginData(PluginEnum.CRYPTOR_REQUEST.getName());
        assertThat(res, is("success"));
    }

    private void cleanRewrite() throws IOException {
        String res = cleanPluginData(PluginEnum.REWRITE.getName());
        assertThat(res, is("success"));
    }

    private void cleanRedirect() throws IOException {
        String res = cleanPluginData(PluginEnum.REDIRECT.getName());
        assertThat(res, is("success"));
    }

    private void cleanRateLimiter() throws IOException {
        String res = cleanPluginData(PluginEnum.RATE_LIMITER.getName());
        assertThat(res, is("success"));
    }

    private void cleanRequest() throws IOException {
        String res = cleanPluginData(PluginEnum.REQUEST.getName());
        assertThat(res, is("success"));
    }

    private void cleanJWT() throws IOException {
        String res = cleanPluginData(PluginEnum.JWT.getName());
        assertThat(res, is("success"));
    }
}
