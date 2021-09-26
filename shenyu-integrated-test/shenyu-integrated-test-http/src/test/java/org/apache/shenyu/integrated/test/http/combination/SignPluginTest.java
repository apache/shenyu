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

import com.google.common.collect.Maps;
import com.google.gson.reflect.TypeToken;
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
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

public final class SignPluginTest extends AbstractPluginDataInit {

    private static final String APP_KEY = "108C27175A2C43C1BC29B1E483D57E3D";

    private static final String APP_SECRET = "061521A73DD94A3FA873C25D050685BB";

    @BeforeClass
    public static void setup() throws IOException {
        String authResult = initAuthData(APP_KEY, APP_SECRET, buildAuthParamDataList(), buildAuthPathDataList());
        assertThat(authResult, is("success"));
        String pluginResult = initPlugin(PluginEnum.SIGN.getName(), null);
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.SIGN.getName(), "", buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
    }

    @Test
    public void testSign() throws Exception {
        final String path = "/http/test/path/456";
        final String testUrlPath = "/http/test/path/456?name=Lee";
        final String version = "1.0.0";
        String now = String.valueOf(LocalDateTime.now().toInstant(ZoneOffset.of("+0")).toEpochMilli());
        Map<String, Object> normalHeaders = buildHeadersMap(now, path, APP_KEY, APP_SECRET, version);
        UserDTO normalRespFuture = HttpHelper.INSTANCE.getFromGateway(testUrlPath, normalHeaders,
                UserDTO.class);
        assertEquals("hello world", normalRespFuture.getUserName());

        Map<String, Object> errorPathHeaders = buildHeadersMap(now, "errorPath", APP_KEY, APP_SECRET, version);
        AdminResponse<Object> rejectedErrorPathRespFuture = HttpHelper.INSTANCE.getFromGateway(testUrlPath,
                errorPathHeaders,
                new TypeToken<AdminResponse<Object>>() {
                }.getType());
        assertEquals("signature value is error!", rejectedErrorPathRespFuture.getMessage());

        Map<String, Object> errorAppKeyHeaders = buildHeadersMap(now, path, "ERRORKEY", APP_SECRET, version);
        AdminResponse<Object> rejectedErrorAKRespFuture = HttpHelper.INSTANCE.getFromGateway(testUrlPath,
                errorAppKeyHeaders,
                new TypeToken<AdminResponse<Object>>() {
                }.getType());
        assertEquals("sign appKey does not exist.", rejectedErrorAKRespFuture.getMessage());

        Map<String, Object> errorAppSecretHeaders = buildHeadersMap(now, path, APP_KEY, "ERRORSECRET", version);
        AdminResponse<Object> rejectedErrorSKRespFuture = HttpHelper.INSTANCE.getFromGateway(testUrlPath,
                errorAppSecretHeaders,
                new TypeToken<AdminResponse<Object>>() {
                }.getType());
        assertEquals("signature value is error!", rejectedErrorSKRespFuture.getMessage());

        Map<String, Object> errorVersionHeaders = buildHeadersMap(now, path, APP_KEY, APP_SECRET, "1.0.2");
        AdminResponse<Object> rejectedErrorVersionRespFuture = HttpHelper.INSTANCE.getFromGateway(testUrlPath,
                errorVersionHeaders,
                new TypeToken<AdminResponse<Object>>() {
                }.getType());
        assertEquals("signature value is error!", rejectedErrorVersionRespFuture.getMessage());

        String errorTime = String.valueOf(LocalDateTime.now().toInstant(ZoneOffset.of("+0")).toEpochMilli() - 360000);
        Map<String, Object> errorTimestampHeaders = buildHeadersMap(errorTime, path, APP_KEY, APP_SECRET, version);
        AdminResponse<Object> rejectedErrorTimestampRespFuture = HttpHelper.INSTANCE.getFromGateway(testUrlPath,
                errorTimestampHeaders,
                new TypeToken<AdminResponse<Object>>() {
                }.getType());
        assertEquals("The signature timestamp has exceeded 5 minutes!", rejectedErrorTimestampRespFuture.getMessage());
    }

    private Map<String, Object> buildHeadersMap(final String timestamp, final String path, final String appKey,
                                                final String appSecret, final String version) {
        Map<String, String> params = Maps.newHashMapWithExpectedSize(3);
        params.put("timestamp", timestamp);
        params.put("path", path);
        params.put("version", version);
        String sign = SignUtils.generateSign(appSecret, params);

        Map<String, Object> headers = Maps.newHashMapWithExpectedSize(4);
        headers.put("timestamp", timestamp);
        headers.put("appKey", appKey);
        headers.put("sign", sign);
        headers.put("version", version);
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
        authPathData.setPath("/http/test/path/456");
        authPathData.setEnabled(true);
        return Collections.singletonList(authPathData);
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/http/test/path/456");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList() {
        final RuleLocalData ruleLocalData = new RuleLocalData();
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/http/test/path/456");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return Collections.singletonList(ruleLocalData);
    }

    @AfterClass
    public static void clean() throws IOException {
        cleanPluginData(PluginEnum.SIGN.getName());
        cleanAuthData(APP_KEY);
    }
}
