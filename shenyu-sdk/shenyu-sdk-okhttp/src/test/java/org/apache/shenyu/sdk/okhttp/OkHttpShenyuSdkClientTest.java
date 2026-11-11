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

package org.apache.shenyu.sdk.okhttp;

import com.sun.net.httpserver.HttpServer;
import okhttp3.ConnectionPool;
import okhttp3.Dispatcher;
import okhttp3.OkHttpClient;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClientFactory;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * The type Ok http shenyu sdk client test.
 */
public class OkHttpShenyuSdkClientTest {

    @Test
    public void testFactoryCreatesIndependentClients() {
        ShenyuSdkClient firstClient = ShenyuSdkClientFactory.newInstance("okhttp");
        ShenyuSdkClient secondClient = ShenyuSdkClientFactory.newInstance("okhttp");

        assertNotSame(firstClient, secondClient);
    }

    @Test
    public void testCloseEvictsOwnedPoolWithoutInitializingDispatcherExecutor() {
        ConnectionPool pool = mock(ConnectionPool.class);
        Dispatcher dispatcher = spy(new Dispatcher());
        OkHttpClient transport = new OkHttpClient.Builder().connectionPool(pool).dispatcher(dispatcher).build();
        OkHttpShenyuSdkClient client = new OkHttpShenyuSdkClient();
        ReflectUtils.setFieldValue(client, "okHttpClient", transport);

        client.close();

        verify(pool).evictAll();
        verify(dispatcher, never()).executorService();
    }

    @Test
    public void testCloseReleasesIdleConnectionsAndAllowsReuse() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/hello", exchange -> {
            byte[] body = "ok".getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Connection", "keep-alive");
            exchange.sendResponseHeaders(200, body.length);
            try (var output = exchange.getResponseBody()) {
                output.write(body);
            }
        });
        server.start();
        OkHttpShenyuSdkClient client = new OkHttpShenyuSdkClient();
        client.initClient(new Properties());
        OkHttpClient transport = (OkHttpClient) ReflectUtils.getFieldValue(client, "okHttpClient");
        try {
            ShenyuRequest request = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET,
                    "http://127.0.0.1:" + server.getAddress().getPort() + "/hello", new HashMap<>(), null, null, null);
            assertEquals("ok", client.doRequest(request).getBody());
            assertTrue(transport.connectionPool().idleConnectionCount() >= 1);

            client.close();

            assertEquals(0, transport.connectionPool().connectionCount());

            assertEquals("ok", client.doRequest(request).getBody());
            assertTrue(transport.connectionPool().idleConnectionCount() >= 1);
            client.close();
            assertEquals(0, transport.connectionPool().connectionCount());
            assertDoesNotThrow(client::close);
        } finally {
            try {
                client.close();
            } finally {
                server.stop(0);
            }
        }
    }

    @Test
    public void testCloseBeforeInitialization() {
        // Edge-case coverage; the initialized-pool test is the regression test.
        assertDoesNotThrow(new OkHttpShenyuSdkClient()::close);
    }

    /**
     * Test shenyu http client.
     *
     */
    @Test
    @Disabled
    public void testShenyuHttpClient() {
        OkHttpShenyuSdkClient okHttpShenyuSdkClient = new OkHttpShenyuSdkClient();
        okHttpShenyuSdkClient.init(new RegisterConfig(), Collections.emptyList(), mock(ShenyuInstanceRegisterRepository.class));
        Map<String, Collection<String>> headerMap = new HashMap<>();
        headerMap.put("header", Arrays.asList("test1", "test2"));
        ShenyuRequest request = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, "https://shenyu.apache.org",
                headerMap, null, null, null);
    }
}
