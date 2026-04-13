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

package org.apache.shenyu.admin.utils;

import com.sun.net.httpserver.HttpServer;
import okhttp3.FormBody;
import okhttp3.HttpUrl;
import okhttp3.Request;
import okhttp3.Response;
import org.junit.Assert;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

import java.net.InetSocketAddress;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

/**
 * HTTP request tool test {@link HttpUtils}.
 */
public class HttpUtilsTest {

    private static final String TEST_URL = "http://127.0.0.1/";

    private final Map<String, Object> formMap = new HashMap<>();

    {
        formMap.put("param-1", "123");
        formMap.put("param-2", 456);
    }

    @Test
    public void buildRequestBuilderForGETTest() {
        Request.Builder builder = HttpUtils.buildRequestBuilder(TEST_URL, formMap, HttpUtils.HTTPMethod.GET);
        Assert.assertNotNull(builder);
        Assert.assertEquals(builder.build().method(), HttpUtils.HTTPMethod.GET.value());
        Assert.assertEquals(TEST_URL, builder.build().url().newBuilder().query(null).build().toString());
        Assert.assertEquals("123", builder.build().url().queryParameter("param-1"));
        Assert.assertEquals("456", builder.build().url().queryParameter("param-2"));
    }

    @Test
    public void buildRequestBuilderForPOSTTest() {
        Request.Builder builder = HttpUtils.buildRequestBuilder(TEST_URL, formMap, HttpUtils.HTTPMethod.POST);
        Assert.assertNotNull(builder);
        Assert.assertNotNull(builder.build().body());
        Assert.assertEquals(builder.build().method(), HttpUtils.HTTPMethod.POST.value());
        Assert.assertEquals(builder.build().url().toString(), TEST_URL);
    }

    @Test
    public void buildRequestBuilderForDELETETest() {
        Request.Builder builder = HttpUtils.buildRequestBuilder(TEST_URL, formMap, HttpUtils.HTTPMethod.DELETE);
        Assert.assertNotNull(builder);
        Assert.assertNotNull(builder.build().body());
        Assert.assertEquals(builder.build().method(), HttpUtils.HTTPMethod.DELETE.value());
        Assert.assertEquals(builder.build().url().toString(), TEST_URL);
    }

    @Test
    public void buildRequestBuilderForPUTTest() {
        Request.Builder builder = HttpUtils.buildRequestBuilder(TEST_URL, formMap, HttpUtils.HTTPMethod.PUT);
        Assert.assertNotNull(builder);
        Assert.assertNotNull(builder.build().body());
        Assert.assertEquals(builder.build().method(), HttpUtils.HTTPMethod.PUT.value());
        Assert.assertEquals(builder.build().url().toString(), TEST_URL);
    }

    @Test
    public void buildRequestBuilderForHEADTest() {
        Request.Builder builder = HttpUtils.buildRequestBuilder(TEST_URL, formMap, HttpUtils.HTTPMethod.HEAD);
        String url = builder.build().url().toString();
        Assert.assertNotNull(builder);
        Assert.assertEquals(builder.build().method(), HttpUtils.HTTPMethod.HEAD.value());
        Assert.assertTrue(url.contains("param-1=123"));
        Assert.assertTrue(url.contains("param-2=456"));
    }

    @Test
    public void buildHttpUrlTest() {
        HttpUrl httpUrl = HttpUtils.buildHttpUrl(TEST_URL, formMap);
        Assert.assertNotNull(httpUrl);
    }

    @Test
    public void buildFormBodyTest() {
        FormBody formBody = HttpUtils.buildFormBody(formMap);
        Assert.assertNotNull(formBody);
    }

    @Test
    public void fileUtilsToBytesByFileExistsTest() {
        String path = HttpUtilsTest.class.getClassLoader().getResource("./").getPath();
        File file = new File(path + "application.yml");
        Assertions.assertDoesNotThrow(() -> HttpUtils.FileUtils.toBytes(file));
    }

    @Test
    public void fileUtilsToBytesByFileNotExistsTest() {
        File file = new File("");
        Assertions.assertThrows(IOException.class, () -> HttpUtils.FileUtils.toBytes(file));
    }

    @Test
    public void requestForResponseShouldNotFollowRedirectsWhenExplicitlyDisabled() throws IOException {
        HttpServer targetServer = HttpServer.create(new InetSocketAddress(0), 0);
        targetServer.createContext("/internal", exchange -> {
            byte[] body = "target".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        targetServer.start();

        HttpServer redirectServer = HttpServer.create(new InetSocketAddress(0), 0);
        String redirectLocation = "http://127.0.0.1:" + targetServer.getAddress().getPort() + "/internal";
        redirectServer.createContext("/swagger.json", exchange -> {
            exchange.getResponseHeaders().add("Location", redirectLocation);
            exchange.sendResponseHeaders(302, -1);
            exchange.close();
        });
        redirectServer.start();

        HttpUtils httpUtils = new HttpUtils();
        String redirectUrl = "http://127.0.0.1:" + redirectServer.getAddress().getPort() + "/swagger.json";
        try (Response response = httpUtils.requestForResponse(redirectUrl, new HashMap<>(), new HashMap<>(), HttpUtils.HTTPMethod.GET, false)) {
            Assert.assertEquals(302, response.code());
            Assert.assertEquals(redirectLocation, response.header("Location"));
        } finally {
            redirectServer.stop(0);
            targetServer.stop(0);
        }
    }

    @Test
    public void requestForResponseShouldFollowRedirectsByDefault() throws IOException {
        HttpServer targetServer = HttpServer.create(new InetSocketAddress(0), 0);
        targetServer.createContext("/internal", exchange -> {
            byte[] body = "target".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        targetServer.start();

        HttpServer redirectServer = HttpServer.create(new InetSocketAddress(0), 0);
        String redirectLocation = "http://127.0.0.1:" + targetServer.getAddress().getPort() + "/internal";
        redirectServer.createContext("/swagger.json", exchange -> {
            exchange.getResponseHeaders().add("Location", redirectLocation);
            exchange.sendResponseHeaders(302, -1);
            exchange.close();
        });
        redirectServer.start();

        HttpUtils httpUtils = new HttpUtils();
        String redirectUrl = "http://127.0.0.1:" + redirectServer.getAddress().getPort() + "/swagger.json";
        try (Response response = httpUtils.requestForResponse(redirectUrl, new HashMap<>(), new HashMap<>(), HttpUtils.HTTPMethod.GET)) {
            Assert.assertEquals(200, response.code());
        } finally {
            redirectServer.stop(0);
            targetServer.stop(0);
        }
    }
}
