/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, VERSION 2.0
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
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.AdminResponse;
import org.apache.shenyu.integratedtest.common.dto.UserDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;

public final class SignPluginTest extends AbstractPluginDataInit {

    private static final String APP_KEY = "108C27175A2C43C1BC29B1E483D57E3D";

    private static final String APP_SECRET = "061521A73DD94A3FA873C25D050685BB";

    private static final String GET_PATH = "/http/test/path/456";
    private static final String POST_PATH = "/http/test/payment";

    private static final String GET_URL = "/http/test/path/456?name=Lee&data=3";
    private static final String POST_URL = "/http/test/payment?userName=Lee&userId=3";
    private static final String VERSION = "1.0.0";

    @BeforeAll
    public static void setUp() throws IOException {
        initAuthData(APP_KEY, APP_SECRET, buildAuthParamDataList(), buildAuthPathDataList());
        initPlugin(PluginEnum.SIGN.getName(), null);
        initSelectorAndRules(PluginEnum.SIGN.getName(), "", buildSelectorConditionList(GET_PATH), buildRuleLocalDataList(false, GET_PATH));
        initSelectorAndRules(PluginEnum.SIGN.getName(), "", buildSelectorConditionList(POST_PATH), buildRuleLocalDataList(true, POST_PATH));
    }

    @Test
    public void testSign() throws Exception {
        String now = String.valueOf(System.currentTimeMillis());
        Map<String, Object> normalHeaders = buildHeadersMap(now, GET_PATH, APP_KEY, APP_SECRET, VERSION, null, null);
        UserDTO normalRespFuture = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, normalHeaders, UserDTO.class);
        assertEquals("Lee", normalRespFuture.getUserName());
    }

    @Test
    public void testSignWithWrongPath() throws Exception {
        String now = String.valueOf(System.currentTimeMillis());
        Map<String, Object> errorPathHeaders = buildHeadersMap(now, "wrong_path", APP_KEY, APP_SECRET, VERSION, null, null);
        AdminResponse<Object> rejectedErrorPathRespFuture = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, errorPathHeaders, AdminResponse.class);
        assertEquals("signature value is error!", rejectedErrorPathRespFuture.getMessage());
    }

    @Test
    public void testSignWithWrongVersion() throws Exception {
        String now = String.valueOf(System.currentTimeMillis());
        Map<String, Object> errorVERSIONHeaders = buildHeadersMap(now, GET_PATH, APP_KEY, APP_SECRET, "1.0.2", null, null);

        AdminResponse<Object> rejectedErrorVERSIONRespFuture = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, errorVERSIONHeaders, AdminResponse.class);

        assertEquals("signature value is error!", rejectedErrorVERSIONRespFuture.getMessage());
    }

    @Test
    public void testSignWithWrongKey() throws Exception {
        String now = String.valueOf(System.currentTimeMillis());
        Map<String, Object> errorAppKeyHeaders = buildHeadersMap(now, GET_PATH, "ERRORKEY", APP_SECRET, VERSION, null, null);

        AdminResponse<Object> rejectedErrorAKRespFuture = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, errorAppKeyHeaders, AdminResponse.class);

        assertEquals("sign appKey does not exist.", rejectedErrorAKRespFuture.getMessage());
    }

    @Test
    public void testSignWithExpiredSignature() throws Exception {

        String errorTime = String.valueOf(System.currentTimeMillis() - 360000);
        Map<String, Object> errorTimestampHeaders = buildHeadersMap(errorTime, GET_PATH, APP_KEY, APP_SECRET, VERSION, null, null);

        AdminResponse<Object> rejectedErrorTimestampRespFuture = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, errorTimestampHeaders, AdminResponse.class);

        assertEquals("The signature timestamp has exceeded 5 minutes!", rejectedErrorTimestampRespFuture.getMessage());
    }

    @Test
    public void testSignWithBodyAndQueryParam() throws Exception {

        String now = String.valueOf(System.currentTimeMillis());
        Map<String, String> requestBody = Maps.newHashMapWithExpectedSize(2);
        requestBody.put("userName", "Lee");
        requestBody.put("userId", "3");
        Map<String, Object> normalHeaders = buildHeadersMap(now, POST_PATH, APP_KEY, APP_SECRET, VERSION, requestBody, requestBody);

        UserDTO userDTO = HttpHelper.INSTANCE
                .postGateway(POST_URL, normalHeaders, requestBody, UserDTO.class);

        assertEquals("Lee", userDTO.getUserName());
        assertEquals("3", userDTO.getUserId());
    }

    @Test
    public void testSignWithWrongBody() throws Exception {

        String now = String.valueOf(System.currentTimeMillis());
        Map<String, String> requestBody = Maps.newHashMapWithExpectedSize(2);
        requestBody.put("userName", "Lee");
        requestBody.put("userId", "3");

        Map<String, Object> errorHeaders = buildHeadersMap(now, POST_PATH, APP_KEY, APP_SECRET, VERSION, ImmutableMap.of("userId", "1234"), requestBody);
        AdminResponse<Object> rejectedErrorVERSIONRespFuture = HttpHelper.INSTANCE
                .postGateway(POST_URL, errorHeaders, requestBody, AdminResponse.class);

        assertEquals("signature value is error!", rejectedErrorVERSIONRespFuture.getMessage());
    }

    @Test
    public void testSignWithIncompleteParam() throws Exception {

        AdminResponse<Object> rejectedErrorPathRespFuture = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, AdminResponse.class);

        assertEquals("sign parameters are incomplete!", rejectedErrorPathRespFuture.getMessage());
    }

    @Test
    public void testSignWithNotConfiguredPath() throws Exception {

        String notConfiguredPath = "/http/test/notConfiguredPath";
        initSelectorAndRules(PluginEnum.SIGN.getName(),
                "",
                buildSelectorConditionList(notConfiguredPath),
                buildRuleLocalDataList(false, notConfiguredPath));
        String now = String.valueOf(System.currentTimeMillis());
        Map<String, Object> errorVERSIONHeaders = buildHeadersMap(now, notConfiguredPath, APP_KEY, APP_SECRET, "1.0.0", null, null);

        AdminResponse<Object> rejectedErrorVERSIONRespFuture = HttpHelper.INSTANCE
                .getFromGateway(notConfiguredPath, errorVERSIONHeaders, AdminResponse.class);

        assertEquals("you have not configured the sign path.",
                rejectedErrorVERSIONRespFuture.getMessage());
    }


    private Map<String, Object> buildHeadersMap(final String timestamp, final String path, final String appKey,
                                                final String appSecret, final String Version, final Map<String, String> queryParams, final Map<String, String> body) {

        String sign = buildSign(appSecret, VERSION, timestamp, path, queryParams, body);
        Map<String, Object> headers = Maps.newHashMapWithExpectedSize(4);
        headers.put("timestamp", timestamp);
        headers.put("appKey", appKey);
        headers.put("sign", sign);
        headers.put("VERSION", VERSION);
        return headers;
    }

    private static List<AuthParamData> buildAuthParamDataList() {
        AuthParamData authParamData = new AuthParamData();
        authParamData.setAppName("http-sign");
        authParamData.setAppParam("appParam");
        return Collections.singletonList(authParamData);
    }

    private static List<AuthPathData> buildAuthPathDataList() {
        AuthPathData authPathData = new AuthPathData();
        authPathData.setAppName("http-sign");
        authPathData.setPath(GET_PATH);
        authPathData.setPath(POST_PATH);
        authPathData.setEnabled(true);
        return Lists.newArrayList(new AuthPathData("http-sign", GET_PATH, true),
                new AuthPathData("http-sign", POST_PATH, true));
    }

    private static List<ConditionData> buildSelectorConditionList(final String path) {
        return buildConditionList(path);
    }

    private static List<RuleLocalData> buildRuleLocalDataList(final boolean signRequestBody, final String path) {
        final RuleLocalData ruleLocalData = new RuleLocalData();
        ruleLocalData.setConditionDataList(buildConditionList(path));
        ruleLocalData.setRuleHandler(String.format("{\"signRequestBody\": %s}", signRequestBody));
        return Collections.singletonList(ruleLocalData);
    }

    private static List<ConditionData> buildConditionList(final String path) {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(path);
        return Collections.singletonList(conditionData);
    }


    @AfterAll
    public static void clean() throws IOException {
        cleanPluginData(PluginEnum.SIGN.getName());
        cleanAuthData(APP_KEY);
    }

    private String buildSign(final String signKey, final String VERSION, final String timeStamp, final String path, final Map<String, String> body, final Map<String, String> queryParams) {

        final String jsonSign = Optional.ofNullable(body).map(e -> e.keySet().stream()
                .sorted(Comparator.naturalOrder())
                .map(key -> String.join("", key, body.get(key)))
                .collect(Collectors.joining()).trim())
                .orElse("");

        final String querySign = Optional.ofNullable(queryParams).map(e -> e.keySet().stream()
                .sorted(Comparator.naturalOrder())
                .map(key -> String.join("", key, queryParams.get(key)))
                .collect(Collectors.joining()).trim())
                .orElse("");

        final String extSignKey = String.join("", Constants.TIMESTAMP, timeStamp, Constants.PATH, path, Constants.VERSION, VERSION, signKey);
        final String data = String.join("", jsonSign, querySign);
        return SignUtils.sign(SignUtils.SIGN_MD5, extSignKey, data).toUpperCase();
    }
}
