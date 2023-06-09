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
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.SignUtils;
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
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_2;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * DefaultSignService Test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class SignServiceVersionTwoTest {

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
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/test-api/demo/test"), null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.success());
    }

    @Test
    public void nullTimestampTest() {

        String parameters = buildParameters(null, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/test-api/demo/test"), null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_PARAMS_ERROR));
    }

    @Test
    public void nullSignTest() {

        String timestamp = String.valueOf(System.currentTimeMillis());
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
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
        String parameters = buildParameters(timestamp, null);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/test-api/demo/test"), null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_PARAMS_ERROR));
    }

    @Test
    public void overdueTest() {
        String errorTimestamp = String.valueOf(System.currentTimeMillis() - ((long) (delay + 1) * 1000 * 60));
        String parameters = buildParameters(errorTimestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/test-api/demo/test"), null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(String.format(ShenyuResultEnum.SIGN_TIME_IS_TIMEOUT.getMsg(), delay)));
    }

    @Test
    public void errorAppKeyTest() {

        String timestamp = String.valueOf(System.currentTimeMillis());
        String parameters = buildParameters(timestamp, "errorKey");
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
                buildSign("errorKey", parameters, URI.create("http://localhost/test-api/demo/test"), null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_APP_KEY_IS_NOT_EXIST));
    }

    @Test
    public void emptyAuthPath() {

        String timestamp = String.valueOf(System.currentTimeMillis());
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/test-api/demo/test"), null));
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
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/test-api/demo/test"), null));
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
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/test-api/demo/test"), null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        AppAuthData authData = SignAuthDataCache.getInstance().obtainAuthData(appKey);
        SignAuthDataCache.getInstance().cacheAuthData(authData);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.success());
    }

    @Test
    public void errorAuthPath() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/errorPath",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/errorPath"), null));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_PATH_NOT_EXIST));
    }

    @Test
    public void errorSign() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters, "errorSign");
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange);
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_VALUE_IS_ERROR));
    }

    @Test
    public void errorBodySign() {
        String timestamp = String.valueOf(System.currentTimeMillis());
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
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
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/test-api/demo/test"), JsonUtils.toJson(requestBody)));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange, JsonUtils.toJson(requestBody));
        assertEquals(ret, VerifyResult.success());
    }

    @Test
    public void bodyAndUrlQueryParamsSign() {
        Map<String, String> requestBody = Maps.newHashMapWithExpectedSize(1);
        requestBody.put("data", "data");

        String timestamp = String.valueOf(System.currentTimeMillis());
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test?data2=data",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/test-api/demo/test?data2=data"), JsonUtils.toJson(requestBody)));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        VerifyResult ret = this.signService.signatureVerify(this.exchange, JsonUtils.toJson(requestBody));
        assertEquals(ret, VerifyResult.success());
    }

    @Test
    public void errorBodyAndUrlQueryParamsSign() {
        Map<String, String> requestBody = Maps.newHashMapWithExpectedSize(1);
        requestBody.put("data", "data");

        String timestamp = String.valueOf(System.currentTimeMillis());
        String parameters = buildParameters(timestamp, appKey);
        this.exchange = buildServerWebExchange("http://localhost/test-api/demo/test?data=data",
                parameters,
                buildSign(secretKey, parameters, URI.create("http://localhost/test-api/demo/test?data=data"), JsonUtils.toJson(requestBody)));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        // Tamper with request body parameters
        requestBody.put("data", "data2");
        VerifyResult ret = this.signService.signatureVerify(this.exchange, JsonUtils.toJson(requestBody));
        assertEquals(ret, VerifyResult.fail(Constants.SIGN_VALUE_IS_ERROR));
    }

    private String buildSign(final String signKey, final String parameters, final URI url, final String body) {

        String data = parameters + getRelativeURL(url) + Optional.ofNullable(body).orElse("");
        return SignUtils.sign(SignUtils.SIGN_MD5, signKey, data).toUpperCase();
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

    private ServerWebExchange buildServerWebExchange(final String url, final String parameters, final String sign) {

        return MockServerWebExchange
                .builder(MockServerHttpRequest.get(url)
                        .header(HttpHeaders.AUTHORIZATION, Objects.isNull(sign) ? parameters : parameters + "." + sign)
                        .header(Constants.VERSION, VERSION_2)).build();
    }

    private String getRelativeURL(final URI uri) {
        if (Objects.isNull(uri.getQuery())) {
            return uri.getPath();
        }
        return uri.getPath() + "?" + uri.getQuery();
    }
}
