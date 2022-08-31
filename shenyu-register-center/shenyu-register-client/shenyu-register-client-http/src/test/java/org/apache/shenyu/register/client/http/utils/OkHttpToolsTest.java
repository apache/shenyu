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

import okhttp3.Call;
import okhttp3.Headers;
import okhttp3.OkHttpClient;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link OkHttpTools}.
 */
public final class OkHttpToolsTest {

    private String url = "http://localhost:9095";

    private String getUrl;

    private String postUrl;

    private final String json = "{\"appName\":\"shenyu\"}";

    private final Map<String, Object> request = new HashMap<>();

    @BeforeEach
    public void setUpWireMock() {
        getUrl = url + "/get";
        postUrl = url + "/post";
    }

    @Test
    public void testPostReturnString() throws IllegalAccessException, NoSuchFieldException, IOException {
        final Field client = OkHttpTools.class.getDeclaredField("client");
        client.setAccessible(true);
        final OkHttpClient okHttpClient = mock(OkHttpClient.class);
        client.set(OkHttpTools.getInstance(), okHttpClient);
        final Call call = mock(Call.class);
        when(okHttpClient.newCall(any())).thenReturn(call);
        final Response response = mock(Response.class);
        when(call.execute()).thenReturn(response);
        final ResponseBody responseBody = mock(ResponseBody.class);
        when(response.body()).thenReturn(responseBody);
        when(responseBody.string()).thenReturn("body");
        Assertions.assertDoesNotThrow(() -> OkHttpTools.getInstance().post(postUrl, json));
        Headers headers = Headers.of().newBuilder().build();
        Assertions.assertDoesNotThrow(() -> OkHttpTools.getInstance().post(postUrl, json, headers));
    }

    @Test
    public void testGetReturnString() throws IllegalAccessException, NoSuchFieldException, IOException {
        final Field client = OkHttpTools.class.getDeclaredField("client");
        client.setAccessible(true);
        final OkHttpClient okHttpClient = mock(OkHttpClient.class);
        client.set(OkHttpTools.getInstance(), okHttpClient);
        final Call call = mock(Call.class);
        when(okHttpClient.newCall(any())).thenReturn(call);
        final Response response = mock(Response.class);
        when(call.execute()).thenReturn(response);
        final ResponseBody responseBody = mock(ResponseBody.class);
        when(response.body()).thenReturn(responseBody);
        when(responseBody.string()).thenReturn("body");
        request.put("get", "request");
        Assertions.assertDoesNotThrow(() -> OkHttpTools.getInstance().get(getUrl, request));
        Assertions.assertDoesNotThrow(() -> OkHttpTools.getInstance().get(getUrl, "userName", "passWord"));
    }
}
