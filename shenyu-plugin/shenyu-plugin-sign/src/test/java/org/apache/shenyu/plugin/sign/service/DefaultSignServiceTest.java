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
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.plugin.api.SignService;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.sign.cache.SignAuthDataCache;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.util.Collections;
import java.util.Map;

/**
 * DefaultSignService Test.
 */
@RunWith(MockitoJUnitRunner.class)
@Slf4j
public final class DefaultSignServiceTest {

    private SignService signService;

    private ServerWebExchange exchange;

    private final String appKey = "D1DFC83F3BC64FABB89DFBD54E5A28C8";

    private final String secretKey = "692C479F98C841FCBEB444B7CA775F63";

    private ShenyuContext passed;

    @Value("${soul.sign.delay:5}")
    private int delay;

    @Before
    public void setup() {
        this.signService = new DefaultSignService();
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());

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
        final String timestamp = String.valueOf(System.currentTimeMillis());
        this.passed.setTimestamp(timestamp);
        this.passed.setSign(buildSign(secretKey, timestamp, this.passed.getPath()));
        this.passed.setAppKey(appKey);
        this.passed.setContextPath("/test-api");
        this.passed.setRealUrl("/demo/test");
    }

    @Test
    public void normalTest() {
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        Assert.assertEquals(ret, Pair.of(true, ""));
    }

    @Test
    public void nullTimestampTest() {
        this.passed.setTimestamp(null);
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        Assert.assertEquals(ret, Pair.of(false, Constants.SIGN_PARAMS_ERROR));
    }

    @Test
    public void nullSignTest() {
        this.passed.setSign(null);
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        Assert.assertEquals(ret, Pair.of(false, Constants.SIGN_PARAMS_ERROR));
    }

    @Test
    public void nullAppKeyTest() {
        this.passed.setAppKey(null);
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        Assert.assertEquals(ret, Pair.of(false, Constants.SIGN_PARAMS_ERROR));
    }

    @Test
    public void overdueTest() {
        Long errorTimestamp = Long.parseLong(this.passed.getTimestamp()) - (long) ((delay + 1) * 1000 * 60);
        this.passed.setTimestamp(errorTimestamp.toString());
        this.passed.setSign(buildSign(secretKey, this.passed.getTimestamp(), this.passed.getPath()));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        Assert.assertEquals(ret, Pair.of(false, String.format(ShenyuResultEnum.SIGN_TIME_IS_TIMEOUT.getMsg(), delay)));
    }

    @Test
    public void errorAppKeyTest() {
        this.passed.setAppKey("errorKey");
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        Assert.assertEquals(ret, Pair.of(false, Constants.SIGN_APP_KEY_IS_NOT_EXIST));
    }

    @Test
    public void emptyAuthPath() {
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        AppAuthData authData = SignAuthDataCache.getInstance().obtainAuthData(appKey);
        authData.setPathDataList(Collections.emptyList());
        SignAuthDataCache.getInstance().cacheAuthData(authData);

        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        Assert.assertEquals(ret, Pair.of(false, Constants.SIGN_PATH_NOT_EXIST));
    }

    @Test
    public void errorAuthPath() {
        this.passed.setPath("errorPath");
        this.passed.setSign(buildSign(secretKey, this.passed.getTimestamp(), this.passed.getPath()));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        Assert.assertEquals(ret, Pair.of(false, Constants.SIGN_PATH_NOT_EXIST));
    }

    @Test
    public void errorSign() {
        this.passed.setSign("errorSign");
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);

        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        Assert.assertEquals(ret, Pair.of(false, Constants.SIGN_VALUE_IS_ERROR));
    }

    private String buildSign(final String signKey, final String timeStamp, final String path) {
        Map<String, String> map = Maps.newHashMapWithExpectedSize(3);
        map.put(Constants.TIMESTAMP, timeStamp);
        map.put(Constants.PATH, path);
        map.put(Constants.VERSION, "1.0.0");
        return SignUtils.generateSign(signKey, map);
    }
}
