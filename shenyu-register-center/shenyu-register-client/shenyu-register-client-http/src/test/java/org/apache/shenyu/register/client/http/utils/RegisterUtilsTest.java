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

package org.apache.shenyu.register.client.http.utils;

import com.google.gson.Gson;
import okhttp3.Headers;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.enums.RegisterTypeEnum;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link RegisterUtils}.
 */
public final class RegisterUtilsTest {

    private final Gson gson = new Gson();

    private OkHttpTools okHttpTools;

    private String json;

    private String url;

    private String accessToken;

    @BeforeEach
    public void setUp() {
        okHttpTools = mock(OkHttpTools.class);
        Map<String, Object> jsonMap = new HashMap<>();
        jsonMap.put("appName", "dubbo");
        jsonMap.put("contextPath", "/dubbo");
        jsonMap.put("path", "/dubbo/findByArrayIdsAndName");
        jsonMap.put("pathDesc", "");
        jsonMap.put("serviceName", "org.apache.shenyu.examples.dubbo.api.service.DubboMultiParamService");
        jsonMap.put("ruleName", "/dubbo/findByArrayIdsAndName");
        jsonMap.put("parameterTypes", "[Ljava.lang.Integer;,java.lang.String");
        jsonMap.put("rpcExt", "{\"group\":\"\",\"version\":\"\",\"loadbalance\":\"random\",\"retries\":2,\"timeout\":10000,\"url\":\"\"}");
        jsonMap.put("enabled", true);
        json = gson.toJson(jsonMap);
        url = "http://localhost:9095/shenyu-client/dubbo-register";
        accessToken = "accessToken";
    }

    @Test
    public void testDoRegisterWhenSuccess() throws IOException {
        when(okHttpTools.post(url, json)).thenReturn("success");
        Headers headers = new Headers.Builder().add(Constants.X_ACCESS_TOKEN, accessToken).build();
        when(okHttpTools.post(url, json, headers)).thenReturn("success");

        try (MockedStatic<OkHttpTools> okHttpToolsMockedStatic = mockStatic(OkHttpTools.class)) {
            okHttpToolsMockedStatic.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
            RegisterUtils.doRegister(json, url, RegisterTypeEnum.DUBBO.getName());
            verify(okHttpTools, times(1)).post(eq(url), eq(json));
            RegisterUtils.doRegister(json, url, RegisterTypeEnum.DUBBO.getName(), accessToken);
            verify(okHttpTools, times(1)).post(eq(url), eq(json), eq(headers));
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"Error parameter！", "{\"code\":400,\"message\":\"invalid parameters\"}"})
    public void testDoRegisterWhenError(final String response) throws IOException {
        when(okHttpTools.post(url, json)).thenReturn(response);
        Headers headers = new Headers.Builder().add(Constants.X_ACCESS_TOKEN, accessToken).build();
        when(okHttpTools.post(url, json, headers)).thenReturn(response);
        try (MockedStatic<OkHttpTools> okHttpToolsMockedStatic = mockStatic(OkHttpTools.class)) {
            okHttpToolsMockedStatic.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
            assertThrows(IOException.class, () -> RegisterUtils.doRegister(json, url, RegisterTypeEnum.DUBBO.getName()));
            verify(okHttpTools, times(1)).post(eq(url), eq(json));
            assertThrows(IOException.class, () -> RegisterUtils.doRegister(json, url, RegisterTypeEnum.DUBBO.getName(), accessToken));
            verify(okHttpTools, times(1)).post(eq(url), eq(json), eq(headers));

            RegisterUtils.doRegister(json, url, RegisterTypeEnum.DUBBO.getName(), null);
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"failure", "{\"code\":400,\"message\":\"invalid parameters\"}"})
    public void testHeartbeatRejectsNonSuccessResponse(final String response) throws IOException {
        Headers headers = new Headers.Builder().add(Constants.X_ACCESS_TOKEN, accessToken).build();
        when(okHttpTools.post(url, json, headers)).thenReturn(response);
        try (MockedStatic<OkHttpTools> mocked = mockStatic(OkHttpTools.class)) {
            mocked.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
            assertThrows(IOException.class, () -> RegisterUtils.doHeartBeat(json, url, Constants.HEARTBEAT, accessToken));
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"failure", "{\"code\":400,\"message\":\"invalid parameters\"}"})
    public void testUnregisterRejectsNonSuccessResponse(final String response) throws IOException {
        Headers headers = new Headers.Builder().add(Constants.X_ACCESS_TOKEN, accessToken).build();
        when(okHttpTools.post(url, json, headers)).thenReturn(response);
        try (MockedStatic<OkHttpTools> mocked = mockStatic(OkHttpTools.class)) {
            mocked.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
            assertThrows(IOException.class, () -> RegisterUtils.doUnregister(json, url, accessToken));
        }
    }

    @Test
    public void testHeartbeatAndUnregisterAcceptSuccess() throws IOException {
        Headers headers = new Headers.Builder().add(Constants.X_ACCESS_TOKEN, accessToken).build();
        when(okHttpTools.post(url, json, headers)).thenReturn(Constants.SUCCESS);
        try (MockedStatic<OkHttpTools> mocked = mockStatic(OkHttpTools.class)) {
            mocked.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
            RegisterUtils.doHeartBeat(json, url, Constants.HEARTBEAT, accessToken);
            RegisterUtils.doUnregister(json, url, accessToken);
            verify(okHttpTools, times(2)).post(url, json, headers);
        }
    }

    @Test
    public void testDoRegisterWhenThrowException() throws IOException {
        when(okHttpTools.post(url, json)).thenThrow(IOException.class);
        assertThrows(IOException.class, () -> {
            try (MockedStatic<OkHttpTools> okHttpToolsMockedStatic = mockStatic(OkHttpTools.class)) {
                okHttpToolsMockedStatic.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
                RegisterUtils.doRegister(json, url, RegisterTypeEnum.DUBBO.getName());
                verify(okHttpTools, times(1)).post(eq(url), eq(json));
            }
        });
    }

    @Test
    public void testDoLogin() throws IOException {
        final String userName = "userName";
        final String password = "password";
        final String token = "token";
        Map<String, Object> loginMap = new HashMap<>(2);
        loginMap.put(Constants.LOGIN_NAME, userName);
        loginMap.put(Constants.PASS_WORD, password);
        when(okHttpTools.post(url, GsonUtils.getInstance().toJson(loginMap))).thenReturn("{\"code\":200,\"data\":{\"token\":\"" + token + "\"}}");
        try (MockedStatic<OkHttpTools> okHttpToolsMockedStatic = mockStatic(OkHttpTools.class)) {
            okHttpToolsMockedStatic.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
            Optional<Object> objectOptional = RegisterUtils.doLogin(userName, password, url);
            Assertions.assertEquals(token, objectOptional.get());
        }
    }

    @Test
    public void testDoLoginError() throws IOException {
        final String userName = "userName";
        final String password = "password";
        Map<String, Object> loginMap = new HashMap<>(2);
        loginMap.put(Constants.LOGIN_NAME, userName);
        loginMap.put(Constants.PASS_WORD, password);
        when(okHttpTools.post(url, GsonUtils.getInstance().toJson(loginMap))).thenReturn("{\"code\":300}");
        try (MockedStatic<OkHttpTools> okHttpToolsMockedStatic = mockStatic(OkHttpTools.class)) {
            okHttpToolsMockedStatic.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
            Optional<Object> objectOptional = RegisterUtils.doLogin(userName, password, url);
            Assertions.assertFalse(objectOptional.isPresent());
        }
    }
}
