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

package org.apache.shenyu.plugin.sign.service;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.DigestUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.sign.api.VerifyResult;
import org.apache.shenyu.plugin.sign.cache.SignAuthDataCache;
import org.apache.shenyu.plugin.sign.extractor.DefaultExtractor;
import org.apache.shenyu.plugin.sign.provider.DefaultSignProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_1;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * DefaultSignService Test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class SignServiceVersionOneTest {

    private SignService signService;

    private ServerWebExchange exchange;

    private final String appKey = "D1DFC83F3BC64FABB89DFBD54E5A28C8";

    private final String secretKey = "692C479F98C841FCBEB444B7CA775F63";

    private ShenyuContext passed;

    @Value("${shenyu.sign.delay:5}")
    private int delay;

    @BeforeEach
    public void setup() {
        this.signService = new ComposableSignService(new DefaultExtractor(), new DefaultSignProvider());

        final String path = "/test-api/demo/test";
        PluginData signData = new PluginData();
        signData.setId("1");
        signData.setName(PluginEnum.SIGN.getName());
        signData.setEnabled(true);
        signData.setRole("1");
        BaseDataCache.getInstance().cachePluginData(signData);

        AppAuthData authData = new AppAuthData();
        authData.setAppKey(appKey);
        authData.setAppSecret(secretKey);
        authData.setEnabled(true);
        authData.setOpen(true);
        AuthPathData authPathData = new AuthPathData();
        authPathData.setAppName("test-api");
        authPathData.setPath(path);
        authPathData.setEnabled(true);
        authData.setPathDataList(Lists.newArrayList(authPathData));
        SignAuthDataCache.getInstance().cacheAuthData(authData);

        this.passed = new ShenyuContext();
        this.passed.setModule("/test-api");
        this.passed.setMethod("/demo/test");
        this.passed.setRpcType("springCloud");
        this.passed.setHttpMethod("GET");
        this.passed.setPath(path);
        this.passed.setContextPath("/test-api");
        this.passed.setRealUrl("/demo/test");

        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @Test
    public void normalTest() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                timestamp,
                appKey,
                buildSign(secretKey, timestamp, "/test-api/demo/test", null, null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.success());
    }

    @Test
    public void nullTimestampTest() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                null,
                appKey,
                buildSign(secretKey, timestamp, "/test-api/demo/test", null, null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_PARAMS_ERROR));
    }

    @Test
    public void nullSignTest() {

        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                timestamp,
                appKey,
                null);
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        AppAuthData authData = SignAuthDataCache.getInstance().obtainAuthData(appKey);
        authData.setPathDataList(Collections.emptyList());
        SignAuthDataCache.getInstance().cacheAuthData(authData);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_PARAMS_ERROR));
    }

    @Test
    public void nullAppKeyTest() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                timestamp,
                null,
                buildSign(secretKey, timestamp, "/test-api/demo/test", null, null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_PARAMS_ERROR));
    }

    @Test
    public void overdueTest() {
        String errorTimestamp = String.valueOf(System.currentTimeMillis() - ((long) (delay + 1) * 1000 * 60));
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                errorTimestamp,
                appKey,
                buildSign(secretKey, errorTimestamp, "/test-api/demo/test", null, null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(String.format(ShenyuResultEnum.SIGN_TIME_IS_TIMEOUT.getMsg(), delay)));
    }

    @Test
    public void errorAppKeyTest() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                timestamp,
                "errorKey",
                buildSign(secretKey, timestamp, "/test-api/demo/test", null, null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_APP_KEY_IS_NOT_EXIST));
    }

    @Test
    public void emptyAuthPath() {

        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                timestamp,
                appKey,
                buildSign(secretKey, timestamp, "/test-api/demo/test", null, null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        AppAuthData authData = SignAuthDataCache.getInstance().obtainAuthData(appKey);
        authData.setPathDataList(Collections.emptyList());
        SignAuthDataCache.getInstance().cacheAuthData(authData);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_PATH_NOT_EXIST));
    }

    @Test
    public void fillParamPath() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                timestamp,
                appKey,
                buildSign(secretKey, timestamp, "/test-api/demo/test", null, null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        AppAuthData authData = SignAuthDataCache.getInstance().obtainAuthData(appKey);
        AuthParamData authParamData = new AuthParamData();
        authParamData.setAppParam("appParam");
        authParamData.setAppName("appParam");
        authData.setParamDataList(Collections.singletonList(authParamData));
        SignAuthDataCache.getInstance().cacheAuthData(authData);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.success());
    }

    @Test
    public void emptyParamPath() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                timestamp,
                appKey,
                buildSign(secretKey, timestamp, "/test-api/demo/test", null, null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        AppAuthData authData = SignAuthDataCache.getInstance().obtainAuthData(appKey);
        SignAuthDataCache.getInstance().cacheAuthData(authData);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.success());
    }

    @Test
    public void errorAuthPath() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/errorPath",
                timestamp,
                appKey,
                buildSign(secretKey, timestamp, "/errorPath", null, null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_PATH_NOT_EXIST));
    }

    @Test
    public void errorSign() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                timestamp,
                appKey,
                "errorSign");
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_VALUE_IS_ERROR));
    }

    @Test
    public void errorBodySign() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                timestamp,
                appKey,
                "errorSign");
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        Map<String, Object> requestBody = Maps.newHashMapWithExpectedSize(1);
        requestBody.put("data", "data");
        VerifyResult ret = this.signService.signatureVerify(this.exchange, JsonUtils.toJson(requestBody));
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_VALUE_IS_ERROR));
    }

    @Test
    public void bodySign() {

        Map<String, String> requestBody = Maps.newHashMapWithExpectedSize(1);
        requestBody.put("data", "data");
        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                timestamp,
                appKey,
                buildSign(secretKey, timestamp, "/test-api/demo/test", requestBody, null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange, JsonUtils.toJson(requestBody));
        assertEquals(ret, VerifyResult.success());
    }

    @Test
    public void bodyAndUrlQueryParamsSign() {
        Map<String, String> requestBody = Maps.newHashMapWithExpectedSize(1);
        requestBody.put("data", "data");
        Map<String, String> queryParams = Maps.newHashMapWithExpectedSize(1);
        queryParams.put("data2", "data");

        String timestamp = String.valueOf(System.currentTimeMillis());
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test?data2=data",
                timestamp,
                appKey,
                buildSign(secretKey, timestamp, "/test-api/demo/test", requestBody, queryParams));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        VerifyResult ret = this.signService.signatureVerify(this.exchange, JsonUtils.toJson(requestBody));
        assertEquals(ret, VerifyResult.success());
    }

    private String buildSign(final String signKey, final String timeStamp, final String path, final Map<String, String> jsonParams, final Map<String, String> queryParams) {

        Map<String, String> params = Maps.newHashMap(Optional.ofNullable(jsonParams)
                .orElse(Collections.EMPTY_MAP));

        params.putAll(Maps.newHashMap(Optional.ofNullable(queryParams)
                .orElse(Collections.EMPTY_MAP)));

        params.put(Constants.TIMESTAMP, timeStamp);
        params.put(Constants.PATH, path);
        params.put(Constants.VERSION, "1.0.0");

        final String sign = params.keySet().stream()
                .sorted(Comparator.naturalOrder())
                .filter(key -> !Objects.equals(key, Constants.SIGN))
                .map(key -> String.join("", key, params.get(key)))
                .collect(Collectors.joining()).trim()
                .concat(signKey);

        return DigestUtils.md5Hex(sign.getBytes()).toUpperCase();
    }

    private MockServerHttpRequest buildMockServerHttpRequest(final String url, final Map<String, String> headers) {
        MockServerHttpRequest.BaseBuilder<?> builder = MockServerHttpRequest.get(url);
        headers.forEach(builder::header);
        return builder.build();
    }

    private ServerWebExchange buildServerWebExchange(final String url, final String timestamp, final String appKey, final String sign) {
        Map<String, String> map = new HashMap<>();
        if (timestamp != null) {
            map.put(Constants.TIMESTAMP, timestamp);
        }
        if (appKey != null) {
            map.put(Constants.APP_KEY, appKey);
        }
        if (sign != null) {
            map.put(Constants.SIGN, sign);
        }
        map.put(Constants.VERSION, VERSION_1);
        return MockServerWebExchange.builder(buildMockServerHttpRequest(url, map)).build();
    }

}
