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

package org.apache.shenyu.sync.data.http;

import okhttp3.Call;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.utils.BoundServerPort;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.junit.jupiter.api.Test;
import org.springframework.boot.web.context.WebServerApplicationContext;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.boot.web.server.WebServer;
import org.springframework.test.util.ReflectionTestUtils;

import java.net.ServerSocket;
import java.util.Collections;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * HTTP long polling reports the bound application port and omits a placeholder.
 */
class HttpSyncBoundPortTest {

    @Test
    void reportsDistinctBoundPortsAndSkipsPlaceholders() throws Exception {
        try (ServerSocket firstSocket = new ServerSocket(0);
                ServerSocket secondSocket = new ServerSocket(0);
                ServerSocket fixedSocket = new ServerSocket(0)) {
            final int firstPort = firstSocket.getLocalPort();
            final int secondPort = secondSocket.getLocalPort();
            final int fixedPort = fixedSocket.getLocalPort();
            assertTrue(firstPort > 0);
            assertNotEquals(firstPort, secondPort);
            HttpSyncDataService first = service();
            HttpSyncDataService second = service();
            try {
                assertNull(poll(first).header("X-Real-PORT"));
                publish(first, 0, null);
                publish(first, secondPort, BoundServerPort.MANAGEMENT_NAMESPACE);
                assertNull(poll(first).header("X-Real-PORT"));
                publish(first, firstPort, null);
                publish(second, secondPort, null);
                publish(second, fixedPort, "server");
                assertEquals(String.valueOf(firstPort), poll(first).header("X-Real-PORT"));
                assertEquals(String.valueOf(fixedPort), poll(second).header("X-Real-PORT"));
                assertNotEquals(poll(first).header("X-Real-PORT"), poll(second).header("X-Real-PORT"));
            } finally {
                first.close();
                second.close();
            }
        }
    }

    private Request poll(final HttpSyncDataService service) {
        AtomicReference<Request> captured = new AtomicReference<>();
        OkHttpClient client = (OkHttpClient) ReflectionTestUtils.getField(service, "okHttpClient");
        when(Objects.requireNonNull(client).newCall(any(Request.class))).thenAnswer(invocation -> {
            captured.set(invocation.getArgument(0));
            return listenerCall();
        });
        ReflectionTestUtils.invokeMethod(service, "doLongPolling", "http://127.0.0.1:1");
        return Objects.requireNonNull(captured.get());
    }

    private void publish(final HttpSyncDataService service, final int port, final String namespace) {
        WebServer webServer = mock(WebServer.class);
        when(webServer.getPort()).thenReturn(port);
        WebServerApplicationContext applicationContext = mock(WebServerApplicationContext.class);
        when(applicationContext.getServerNamespace()).thenReturn(namespace);
        WebServerInitializedEvent event = mock(WebServerInitializedEvent.class);
        when(event.getWebServer()).thenReturn(webServer);
        when(event.getApplicationContext()).thenReturn(applicationContext);
        service.onWebServerInitialized(event);
    }

    private HttpSyncDataService service() throws Exception {
        OkHttpClient client = mock(OkHttpClient.class);
        when(client.newCall(any(Request.class))).thenAnswer(invocation -> fetchCall());
        HttpConfig httpConfig = new HttpConfig();
        httpConfig.setUrl("http://127.0.0.1:1");
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.setNamespace("shenyu");
        AccessTokenManager accessTokenManager = mock(AccessTokenManager.class);
        when(accessTokenManager.getAccessToken()).thenReturn("token");
        HttpSyncDataService service = new HttpSyncDataService(httpConfig, mock(PluginDataSubscriber.class), client,
                Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(),
                Collections.emptyList(), accessTokenManager, shenyuConfig);
        ExecutorService executor = (ExecutorService) ReflectionTestUtils.getField(service, "executor");
        service.close();
        if (Objects.nonNull(executor)) {
            executor.awaitTermination(2, TimeUnit.SECONDS);
        }
        return service;
    }

    private Call fetchCall() throws Exception {
        Call call = mock(Call.class);
        Response response = mock(Response.class);
        ResponseBody body = mock(ResponseBody.class);
        when(response.isSuccessful()).thenReturn(true);
        when(response.body()).thenReturn(body);
        when(body.string()).thenReturn("{\"code\":200,\"data\":{}}");
        when(call.execute()).thenReturn(response);
        return call;
    }

    private Call listenerCall() throws Exception {
        Call call = mock(Call.class);
        Response response = mock(Response.class);
        ResponseBody body = mock(ResponseBody.class);
        when(response.isSuccessful()).thenReturn(true);
        when(response.body()).thenReturn(body);
        when(body.string()).thenReturn("{\"data\":null}");
        when(call.execute()).thenReturn(response);
        return call;
    }
}
