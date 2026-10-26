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

package org.apache.shenyu.sdk.httpclient;

import com.sun.net.httpserver.HttpServer;
import org.apache.http.impl.nio.client.CloseableHttpAsyncClient;
import org.apache.http.impl.nio.conn.PoolingNHttpClientConnectionManager;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClientFactory;
import org.apache.http.HttpResponse;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.concurrent.FutureCallback;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.Mockito;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.Assert.assertNotSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class HttpShenyuSdkClientTest {

    @Test(timeout = 15000)
    public void testShenyuHttpClient() throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        HttpShenyuSdkClient shenyuHttpClient = new HttpShenyuSdkClient();
        AtomicReference<String> requestMethod = new AtomicReference<>();
        AtomicReference<String> requestUri = new AtomicReference<>();
        AtomicReference<List<String>> requestHeaders = new AtomicReference<>();
        String responseBody = "{\"message\":\"本地响应\"}";
        try {
            server.createContext("/sdk", exchange -> {
                try {
                    requestMethod.set(exchange.getRequestMethod());
                    requestUri.set(exchange.getRequestURI().toString());
                    requestHeaders.set(exchange.getRequestHeaders().get("header"));
                    byte[] bytes = responseBody.getBytes(StandardCharsets.UTF_8);
                    exchange.getResponseHeaders().set("Content-Type", "application/json; charset=UTF-8");
                    exchange.getResponseHeaders().set("X-Fixture", "local");
                    exchange.sendResponseHeaders(201, bytes.length);
                    exchange.getResponseBody().write(bytes);
                } finally {
                    exchange.close();
                }
            });
            server.start();
            Properties props = new Properties();
            props.setProperty("http.serverRequestTimeOut", "5000");
            props.setProperty("http.serverResponseTimeOut", "5000");
            shenyuHttpClient.initClient(props);
            Map<String, Collection<String>> headerMap = new HashMap<>();
            headerMap.put("header", Arrays.asList("test1", "test2"));
            String url = "http://127.0.0.1:" + server.getAddress().getPort() + "/sdk?source=local";
            ShenyuRequest shenyuRequest = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, url,
                    headerMap, null, null, null);

            ShenyuResponse response = shenyuHttpClient.doRequest(shenyuRequest);

            Assert.assertEquals("GET", requestMethod.get());
            Assert.assertEquals("/sdk?source=local", requestUri.get());
            Assert.assertEquals(Arrays.asList("test1", "test2"), requestHeaders.get());
            Assert.assertEquals(201, response.getStatus());
            Assert.assertEquals(responseBody, response.getBody());
            Assert.assertSame(shenyuRequest, response.getRequest());
            Assert.assertTrue(response.getHeaders().entrySet().stream()
                    .anyMatch(header -> "X-Fixture".equalsIgnoreCase(header.getKey()) && header.getValue().contains("local")));
        } finally {
            try {
                shenyuHttpClient.close();
            } finally {
                server.stop(0);
            }
        }
    }

    @Test
    public void testClose() throws Exception {
        HttpShenyuSdkClient shenyuHttpClient = new HttpShenyuSdkClient();
        CloseableHttpAsyncClient httpAsyncClient = mock(CloseableHttpAsyncClient.class);
        PoolingNHttpClientConnectionManager connectionManager = mock(PoolingNHttpClientConnectionManager.class);
        setField(shenyuHttpClient, "httpAsyncClient", httpAsyncClient);
        setField(shenyuHttpClient, "connectionManager", connectionManager);

        shenyuHttpClient.close();

        verify(httpAsyncClient).close();
        verify(connectionManager).shutdown();
    }

    @Test
    public void testConnectionRequestTimeoutHonorsConfiguredProperty() throws Exception {
        HttpShenyuSdkClient client = new HttpShenyuSdkClient();
        Properties props = new Properties();
        props.setProperty("http.connectionRequestTimeOut", "50");
        try {
            client.initClient(props);
            RequestConfig requestConfig = (RequestConfig) getField(client, "requestConfig");
            Assert.assertEquals(50, requestConfig.getConnectionRequestTimeout());
        } finally {
            client.close();
        }
    }

    @Test
    public void testConnectionRequestTimeoutDefaultsWhenUnset() throws Exception {
        HttpShenyuSdkClient client = new HttpShenyuSdkClient();
        try {
            client.initClient(new Properties());
            RequestConfig requestConfig = (RequestConfig) getField(client, "requestConfig");
            Assert.assertEquals(2000, requestConfig.getConnectionRequestTimeout());
        } finally {
            client.close();
        }
    }

    @Test
    public void testFactoryCreatesIndependentClients() {
        ShenyuSdkClient firstClient = ShenyuSdkClientFactory.newInstance("httpclient");
        ShenyuSdkClient secondClient = ShenyuSdkClientFactory.newInstance("httpclient");

        assertNotSame(firstClient, secondClient);
    }

    private Object getField(final HttpShenyuSdkClient client, final String name) throws Exception {
        Field field = HttpShenyuSdkClient.class.getDeclaredField(name);
        field.setAccessible(true);
        return field.get(client);
    }

    private void setField(final HttpShenyuSdkClient client, final String name, final Object value) throws Exception {
        Field field = HttpShenyuSdkClient.class.getDeclaredField(name);
        field.setAccessible(true);
        field.set(client, value);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testTransportFailureThrowsIOException() throws Exception {
        HttpShenyuSdkClient shenyuHttpClient = new HttpShenyuSdkClient();
        CloseableHttpAsyncClient httpAsyncClient = mock(CloseableHttpAsyncClient.class);
        Future<HttpResponse> future = mock(Future.class);
        when(httpAsyncClient.execute(Mockito.any(HttpUriRequest.class), Mockito.<FutureCallback<HttpResponse>>any())).thenReturn(future);
        when(future.get()).thenThrow(new ExecutionException(new IOException("connection reset")));
        setHttpAsyncClient(shenyuHttpClient, httpAsyncClient);

        Assert.assertThrows(IOException.class, () -> shenyuHttpClient.doRequest(createRequest()));
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testInterruptedFailureRestoresInterruptStatus() throws Exception {
        HttpShenyuSdkClient shenyuHttpClient = new HttpShenyuSdkClient();
        CloseableHttpAsyncClient httpAsyncClient = mock(CloseableHttpAsyncClient.class);
        Future<HttpResponse> future = mock(Future.class);
        when(httpAsyncClient.execute(Mockito.any(HttpUriRequest.class), Mockito.<FutureCallback<HttpResponse>>any())).thenReturn(future);
        when(future.get()).thenThrow(new InterruptedException("interrupted"));
        setHttpAsyncClient(shenyuHttpClient, httpAsyncClient);

        try {
            Assert.assertThrows(IOException.class, () -> shenyuHttpClient.doRequest(createRequest()));
            Assert.assertTrue(Thread.currentThread().isInterrupted());
        } finally {
            Thread.interrupted();
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testCancelledRequestThrowsIOException() throws Exception {
        HttpShenyuSdkClient shenyuHttpClient = new HttpShenyuSdkClient();
        CloseableHttpAsyncClient httpAsyncClient = mock(CloseableHttpAsyncClient.class);
        Future<HttpResponse> future = mock(Future.class);
        when(httpAsyncClient.execute(Mockito.any(HttpUriRequest.class), Mockito.<FutureCallback<HttpResponse>>any())).thenReturn(future);
        when(future.get()).thenThrow(new CancellationException("cancelled"));
        setHttpAsyncClient(shenyuHttpClient, httpAsyncClient);

        Assert.assertThrows(IOException.class, () -> shenyuHttpClient.doRequest(createRequest()));
    }

    private ShenyuRequest createRequest() {
        return ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, "http://localhost/test",
                new HashMap<>(), null, null, null);
    }

    private void setHttpAsyncClient(final HttpShenyuSdkClient client, final CloseableHttpAsyncClient httpAsyncClient) throws Exception {
        Field field = HttpShenyuSdkClient.class.getDeclaredField("httpAsyncClient");
        field.setAccessible(true);
        field.set(client, httpAsyncClient);
    }

}
