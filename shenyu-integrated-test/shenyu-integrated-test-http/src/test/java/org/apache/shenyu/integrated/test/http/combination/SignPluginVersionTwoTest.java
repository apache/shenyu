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
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.AdminResponse;
import org.apache.shenyu.integratedtest.common.dto.UserDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import static org.apache.shenyu.integratedtest.common.helper.HttpHelper.GATEWAY_END_POINT;
import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_2;
import static org.junit.jupiter.api.Assertions.assertEquals;

public final class SignPluginVersionTwoTest extends AbstractPluginDataInit {

    private static final String APP_KEY = "108C27175A2C43C1BC29B1E483D57E3D";

    private static final String APP_SECRET = "061521A73DD94A3FA873C25D050685BB";

    private static final String GET_PATH = "/http/test/path/456";

    private static final String POST_PATH = "/http/test/payment";

    private static final String GET_URL = "/http/test/path/456?name=Lee&data=3";

    private static final String POST_URL = "/http/test/payment?userName=Lee&userId=3";

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
        Map<String, Object> normalHeaders = buildHeadersMap(GATEWAY_END_POINT + GET_URL, now, APP_KEY, APP_SECRET, null);
        UserDTO result = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, normalHeaders, UserDTO.class);
        assertEquals("Lee", result.getUserName());
    }

    @Test
    public void testSignWithWrongPath() throws Exception {
        String now = String.valueOf(System.currentTimeMillis());
        Map<String, Object> errorPathHeaders = buildHeadersMap(GATEWAY_END_POINT + "/wrong_path", now, APP_KEY, APP_SECRET, null);
        AdminResponse<Object> result = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, errorPathHeaders, AdminResponse.class);
        assertEquals("signature value is error!", result.getMessage());
    }

    @Test
    public void testSignWithWrongKey() throws Exception {
        String now = String.valueOf(System.currentTimeMillis());
        Map<String, Object> headers = buildHeadersMap(GATEWAY_END_POINT + GET_URL, now, "ERRORKEY", APP_SECRET, null);

        AdminResponse<Object> result = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, headers, AdminResponse.class);

        assertEquals("sign appKey does not exist.", result.getMessage());
    }

    @Test
    public void testSignWithExpiredSignature() throws Exception {

        String errorTime = String.valueOf(System.currentTimeMillis() - 360000);
        Map<String, Object> headers = buildHeadersMap(GATEWAY_END_POINT + GET_URL, errorTime, APP_KEY, APP_SECRET, null);

        AdminResponse<Object> result = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, headers, AdminResponse.class);

        assertEquals("The signature timestamp has exceeded 5 minutes!", result.getMessage());
    }

    @Test
    public void testSignWithBodyAndQueryParam() throws Exception {

        String now = String.valueOf(System.currentTimeMillis());
        Map<String, String> requestBody = Maps.newHashMapWithExpectedSize(2);
        requestBody.put("userName", "Lee");
        requestBody.put("userId", "3");
        Map<String, Object> headers = buildHeadersMap(GATEWAY_END_POINT + POST_URL, now, APP_KEY, APP_SECRET, JsonUtils.toJson(requestBody));

        UserDTO result = HttpHelper.INSTANCE
                .postGateway(POST_URL, headers, requestBody, UserDTO.class);

        assertEquals("Lee", result.getUserName());
        assertEquals("3", result.getUserId());
    }

    @Test
    public void testSignWithWrongBody() throws Exception {

        String now = String.valueOf(System.currentTimeMillis());
        Map<String, String> requestBody = Maps.newHashMapWithExpectedSize(2);
        requestBody.put("userName", "Lee");
        requestBody.put("userId", "3");

        Map<String, Object> headers = buildHeadersMap(GATEWAY_END_POINT + POST_URL, now, APP_KEY, APP_SECRET, JsonUtils.toJson(ImmutableMap.of("userId", "1234")));
        AdminResponse<Object> result = HttpHelper.INSTANCE
                .postGateway(POST_URL, headers, requestBody, AdminResponse.class);

        assertEquals("signature value is error!", result.getMessage());
    }

    @Test
    public void testSignWithIncompleteParam() throws Exception {

        Map<String, Object> headers = buildHeadersMap(GATEWAY_END_POINT + POST_URL,
                null, APP_KEY, APP_SECRET, null);

        AdminResponse<Object> result = HttpHelper.INSTANCE
                .getFromGateway(GET_URL, headers, AdminResponse.class);

        assertEquals("sign parameters are incomplete!", result.getMessage());
    }

    @Test
    public void testSignWithNotConfiguredPath() throws Exception {

        String notConfiguredPath = "/http/test/notConfiguredPath";
        initSelectorAndRules(PluginEnum.SIGN.getName(),
                "",
                buildSelectorConditionList(notConfiguredPath),
                buildRuleLocalDataList(false, notConfiguredPath));
        String now = String.valueOf(System.currentTimeMillis());
        Map<String, Object> headers = buildHeadersMap(GATEWAY_END_POINT + notConfiguredPath, now, APP_KEY, APP_SECRET, null);

        AdminResponse<Object> result = HttpHelper.INSTANCE
                .getFromGateway(notConfiguredPath, headers, AdminResponse.class);

        assertEquals("you have not configured the sign path.",
                result.getMessage());
    }

    @Test
    private Map<String, Object> buildHeadersMap(final String uri, final String timestamp, final String appKey,
                                                final String appSecret, final String requestBody) {
        String parameters = buildParameters(timestamp, appKey);
        String sign = buildSign(appSecret, parameters, URI.create(uri), requestBody);
        String token = parameters + "." + sign;
        return ImmutableMap.of(HttpHeaders.AUTHORIZATION, token, Constants.VERSION, VERSION_2);
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

    private String buildParameters(final String timestamp, final String appKey) {
        Map<String, String> map = new HashMap<>();
        if (timestamp != null) {
            map.put(Constants.TIMESTAMP, timestamp);
        }
        if (appKey != null) {
            map.put(Constants.APP_KEY, appKey);
        }
        map.put("alg", "MD5");
        return Base64.getEncoder().encodeToString(JsonUtils.toJson(map).getBytes(StandardCharsets.UTF_8));
    }

    private String buildSign(final String signKey, final String parameters, final URI url, final String body) {

        String data = parameters + getRelativeURL(url) + Optional.ofNullable(body).orElse("");
        return SignUtils.sign(SignUtils.SIGN_MD5, signKey, data).toUpperCase();
    }

    private String getRelativeURL(final URI uri) {
        if (Objects.isNull(uri.getQuery())) {
            return uri.getPath();
        }
        return uri.getPath() + "?" + uri.getQuery();
    }
}
