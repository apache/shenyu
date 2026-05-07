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
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import okhttp3.FormBody;
import okhttp3.HttpUrl;
import okhttp3.Request;
import okhttp3.Response;
import org.junit.Assert;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Test case for {@link HttpUtils}.
 */
public class HttpUtilsTest {

    private static final String TEST_URL = "http://127.0.0.1/";

    private static final String LOCALHOST_BASE_URL = "http://127.0.0.1:";

    private static final String REDIRECT_PATH = "/swagger.json";

    private static final String TARGET_PATH = "/internal";

    private static final String TARGET_BODY = "target";

    private static final int HTTP_STATUS_OK = 200;

    private static final int HTTP_STATUS_REDIRECT = 302;

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
        targetServer.createContext(TARGET_PATH, exchange -> {
            byte[] body = TARGET_BODY.getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(HTTP_STATUS_OK, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        targetServer.start();

        HttpServer redirectServer = HttpServer.create(new InetSocketAddress(0), 0);
        String redirectLocation = LOCALHOST_BASE_URL + targetServer.getAddress().getPort() + TARGET_PATH;
        redirectServer.createContext(REDIRECT_PATH, exchange -> {
            exchange.getResponseHeaders().add("Location", redirectLocation);
            exchange.sendResponseHeaders(HTTP_STATUS_REDIRECT, -1);
            exchange.close();
        });
        redirectServer.start();

        HttpUtils httpUtils = new HttpUtils();
        String redirectUrl = LOCALHOST_BASE_URL + redirectServer.getAddress().getPort() + REDIRECT_PATH;
        try (Response response = httpUtils.requestForResponse(redirectUrl, new HashMap<>(), new HashMap<>(), HttpUtils.HTTPMethod.GET, false)) {
            Assert.assertEquals(HTTP_STATUS_REDIRECT, response.code());
            Assert.assertEquals(redirectLocation, response.header("Location"));
        } finally {
            redirectServer.stop(0);
            targetServer.stop(0);
        }
    }

    @Test
    public void requestForResponseShouldFollowRedirectsByDefault() throws IOException {
        HttpServer targetServer = HttpServer.create(new InetSocketAddress(0), 0);
        targetServer.createContext(TARGET_PATH, exchange -> {
            byte[] body = TARGET_BODY.getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(HTTP_STATUS_OK, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        targetServer.start();

        HttpServer redirectServer = HttpServer.create(new InetSocketAddress(0), 0);
        String redirectLocation = LOCALHOST_BASE_URL + targetServer.getAddress().getPort() + TARGET_PATH;
        redirectServer.createContext(REDIRECT_PATH, exchange -> {
            exchange.getResponseHeaders().add("Location", redirectLocation);
            exchange.sendResponseHeaders(HTTP_STATUS_REDIRECT, -1);
            exchange.close();
        });
        redirectServer.start();

        HttpUtils httpUtils = new HttpUtils();
        String redirectUrl = LOCALHOST_BASE_URL + redirectServer.getAddress().getPort() + REDIRECT_PATH;
        try (Response response = httpUtils.requestForResponse(redirectUrl, new HashMap<>(), new HashMap<>(), HttpUtils.HTTPMethod.GET)) {
            Assert.assertEquals(HTTP_STATUS_OK, response.code());
        } finally {
            redirectServer.stop(0);
            targetServer.stop(0);
        }
    }

    @Test
    public void constructorWithHttpToolConfigTest() {
        HttpUtils.HttpToolConfig config = new HttpUtils.HttpToolConfig();
        config.setConnectTimeoutSeconds(5);
        config.setReadTimeoutSeconds(5);
        config.setWriteTimeoutSeconds(5);
        HttpUtils httpUtils = new HttpUtils(config);
        Assert.assertNotNull(httpUtils);
    }

    @Test
    public void httpToolConfigGettersAndSettersTest() {
        HttpUtils.HttpToolConfig config = new HttpUtils.HttpToolConfig();
        config.setConnectTimeoutSeconds(10);
        config.setReadTimeoutSeconds(15);
        config.setWriteTimeoutSeconds(20);
        Assert.assertEquals(10, config.getConnectTimeoutSeconds());
        Assert.assertEquals(15, config.getReadTimeoutSeconds());
        Assert.assertEquals(20, config.getWriteTimeoutSeconds());
    }

    @Test
    public void httpMethodFromValueTest() {
        Assert.assertEquals(HttpUtils.HTTPMethod.GET, HttpUtils.HTTPMethod.fromValue("GET"));
        Assert.assertEquals(HttpUtils.HTTPMethod.POST, HttpUtils.HTTPMethod.fromValue("post"));
        Assert.assertEquals(HttpUtils.HTTPMethod.PUT, HttpUtils.HTTPMethod.fromValue("Put"));
    }

    @Test
    public void httpMethodValueTest() {
        Assert.assertEquals("GET", HttpUtils.HTTPMethod.GET.value());
        Assert.assertEquals("POST", HttpUtils.HTTPMethod.POST.value());
        Assert.assertEquals("PUT", HttpUtils.HTTPMethod.PUT.value());
        Assert.assertEquals("DELETE", HttpUtils.HTTPMethod.DELETE.value());
        Assert.assertEquals("HEAD", HttpUtils.HTTPMethod.HEAD.value());
    }

    @Test
    public void uploadFileConstructorWithFileTest() throws IOException {
        File tempFile = File.createTempFile("test", ".txt");
        tempFile.deleteOnExit();
        HttpUtils.UploadFile uploadFile = new HttpUtils.UploadFile("test", tempFile);
        Assert.assertNotNull(uploadFile);
        Assert.assertEquals("test", uploadFile.getName());
        Assert.assertEquals(tempFile.getName(), uploadFile.getFileName());
        Assert.assertNotNull(uploadFile.getFileData());
    }

    @Test
    public void uploadFileConstructorWithInputStreamTest() throws IOException {
        String testData = "test data";
        ByteArrayInputStream inputStream = new ByteArrayInputStream(testData.getBytes());
        HttpUtils.UploadFile uploadFile = new HttpUtils.UploadFile("test", "test.txt", inputStream);
        Assert.assertNotNull(uploadFile);
        Assert.assertEquals("test", uploadFile.getName());
        Assert.assertEquals("test.txt", uploadFile.getFileName());
        Assert.assertArrayEquals(testData.getBytes(), uploadFile.getFileData());
    }

    @Test
    public void uploadFileConstructorWithByteArrayTest() {
        byte[] data = "test data".getBytes();
        HttpUtils.UploadFile uploadFile = new HttpUtils.UploadFile("test", "test.txt", data);
        Assert.assertNotNull(uploadFile);
        Assert.assertEquals("test", uploadFile.getName());
        Assert.assertEquals("test.txt", uploadFile.getFileName());
        Assert.assertArrayEquals(data, uploadFile.getFileData());
        Assert.assertNotNull(uploadFile.getMd5());
    }

    @Test
    public void uploadFileGettersAndSettersTest() {
        HttpUtils.UploadFile uploadFile = new HttpUtils.UploadFile("test", "test.txt", "data".getBytes());
        uploadFile.setName("newName");
        uploadFile.setFileName("newFile.txt");
        uploadFile.setFileData("newData".getBytes());
        uploadFile.setMd5("newMd5");
        Assert.assertEquals("newName", uploadFile.getName());
        Assert.assertEquals("newFile.txt", uploadFile.getFileName());
        Assert.assertArrayEquals("newData".getBytes(), uploadFile.getFileData());
        Assert.assertEquals("newMd5", uploadFile.getMd5());
    }

    @Test
    public void fileUtilsToBytesByInputStreamTest() throws IOException {
        String testData = "test data";
        ByteArrayInputStream inputStream = new ByteArrayInputStream(testData.getBytes());
        byte[] result = HttpUtils.FileUtils.toBytes(inputStream);
        Assert.assertArrayEquals(testData.getBytes(), result);
    }

    @Test
    public void getRequestTest() throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/test", exchange -> {
            String response = "Hello World";
            exchange.sendResponseHeaders(200, response.length());
            exchange.getResponseBody().write(response.getBytes());
            exchange.close();
        });
        server.start();

        HttpUtils httpUtils = new HttpUtils();
        String url = LOCALHOST_BASE_URL + server.getAddress().getPort() + "/test";
        try {
            String result = httpUtils.get(url, new HashMap<>());
            Assert.assertEquals("Hello World", result);
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void requestMethodTest() throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/test", exchange -> {
            String response = "POST response";
            exchange.sendResponseHeaders(200, response.length());
            exchange.getResponseBody().write(response.getBytes());
            exchange.close();
        });
        server.start();

        HttpUtils httpUtils = new HttpUtils();
        String url = LOCALHOST_BASE_URL + server.getAddress().getPort() + "/test";
        try {
            String result = httpUtils.request(url, new HashMap<>(), new HashMap<>(), HttpUtils.HTTPMethod.POST);
            Assert.assertEquals("POST response", result);
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void requestJsonMethodTest() throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/test", exchange -> {
            String response = "{\"status\":\"ok\"}";
            exchange.sendResponseHeaders(200, response.length());
            exchange.getResponseBody().write(response.getBytes());
            exchange.close();
        });
        server.start();

        HttpUtils httpUtils = new HttpUtils();
        String url = LOCALHOST_BASE_URL + server.getAddress().getPort() + "/test";
        Map<String, String> headers = new HashMap<>();
        headers.put("Content-Type", "application/json");
        try {
            Response response = httpUtils.requestJson(url, "{\"test\":\"data\"}", headers, HttpUtils.HTTPMethod.POST);
            Assert.assertEquals(200, response.code());
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void requestFileStringMethodTest() throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/test", exchange -> {
            String response = "File uploaded";
            exchange.sendResponseHeaders(200, response.length());
            exchange.getResponseBody().write(response.getBytes());
            exchange.close();
        });
        server.start();

        HttpUtils httpUtils = new HttpUtils();
        String url = LOCALHOST_BASE_URL + server.getAddress().getPort() + "/test";
        List<HttpUtils.UploadFile> files = new ArrayList<>();
        files.add(new HttpUtils.UploadFile("file", "test.txt", "content".getBytes()));
        try {
            String result = httpUtils.requestFileString(url, new HashMap<>(), new HashMap<>(), files);
            Assert.assertEquals("File uploaded", result);
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void requestFileMethodTest() throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/test", exchange -> {
            String response = "File uploaded";
            exchange.sendResponseHeaders(200, response.length());
            exchange.getResponseBody().write(response.getBytes());
            exchange.close();
        });
        server.start();

        HttpUtils httpUtils = new HttpUtils();
        String url = LOCALHOST_BASE_URL + server.getAddress().getPort() + "/test";
        List<HttpUtils.UploadFile> files = new ArrayList<>();
        files.add(new HttpUtils.UploadFile("file", "test.txt", "content".getBytes()));
        try {
            Response response = httpUtils.requestFile(url, new HashMap<>(), new HashMap<>(), files);
            Assert.assertEquals(200, response.code());
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void requestCallMethodTest() throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/test", exchange -> {
            String response = "Call response";
            exchange.sendResponseHeaders(200, response.length());
            exchange.getResponseBody().write(response.getBytes());
            exchange.close();
        });
        server.start();

        HttpUtils httpUtils = new HttpUtils();
        String url = LOCALHOST_BASE_URL + server.getAddress().getPort() + "/test";
        List<HttpUtils.UploadFile> files = new ArrayList<>();
        files.add(new HttpUtils.UploadFile("file", "test.txt", "content".getBytes()));
        try {
            Response response = httpUtils.requestCall(url, new HashMap<>(), new HashMap<>(), HttpUtils.HTTPMethod.POST, files);
            Assert.assertEquals(200, response.code());
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void downloadFileMethodTest() throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/test", exchange -> {
            String response = "File content";
            exchange.sendResponseHeaders(200, response.length());
            exchange.getResponseBody().write(response.getBytes());
            exchange.close();
        });
        server.start();

        HttpUtils httpUtils = new HttpUtils();
        String url = LOCALHOST_BASE_URL + server.getAddress().getPort() + "/test";
        try {
            InputStream inputStream = httpUtils.downloadFile(url, new HashMap<>(), new HashMap<>());
            Assert.assertNotNull(inputStream);
            // Note: Due to the current implementation closing the response immediately,
            // we can only verify the stream is returned, not read its content
            // This is a limitation of the current downloadFile implementation
        } finally {
            server.stop(0);
        }
    }
}
