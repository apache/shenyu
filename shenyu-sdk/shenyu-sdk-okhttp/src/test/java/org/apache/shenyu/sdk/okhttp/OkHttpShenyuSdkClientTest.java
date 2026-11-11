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
import okhttp3.OkHttpClient;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClientFactory;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

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
    public void testCloseReleasesIdleConnections() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/hello", exchange -> {
            byte[] body = "ok".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, body.length);
            try (var output = exchange.getResponseBody()) {
                output.write(body);
            }
        });
        server.start();
        OkHttpShenyuSdkClient client = new OkHttpShenyuSdkClient();
        client.initClient(new Properties());
        Field field = OkHttpShenyuSdkClient.class.getDeclaredField("okHttpClient");
        field.setAccessible(true);
        OkHttpClient transport = (OkHttpClient) field.get(client);
        try {
            ShenyuRequest request = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET,
                    "http://127.0.0.1:" + server.getAddress().getPort() + "/hello", new HashMap<>(), null, null, null);
            assertEquals("ok", client.doRequest(request).getBody());
            assertEquals(1, transport.connectionPool().idleConnectionCount());
            assertFalse(transport.dispatcher().executorService().isShutdown());

            client.close();

            assertEquals(0, transport.connectionPool().connectionCount());
            assertTrue(transport.dispatcher().executorService().isShutdown());
            assertDoesNotThrow(client::close);
        } finally {
            transport.connectionPool().evictAll();
            transport.dispatcher().executorService().shutdown();
            server.stop(0);
        }
    }

    @Test
    public void testCloseBeforeInitialization() {
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
