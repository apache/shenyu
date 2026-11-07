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
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Request media types and body bytes sent by the real OkHttp SDK transport.
 */
@Timeout(15)
public final class OkHttpRequestContentTypeTest {

    private HttpServer server;

    private OkHttpShenyuSdkClient client;

    private final AtomicReference<String> receivedMethod = new AtomicReference<>();

    private final AtomicReference<String> receivedContentType = new AtomicReference<>();

    private final AtomicReference<byte[]> receivedBody = new AtomicReference<>();

    private final AtomicReference<List<String>> receivedTrace = new AtomicReference<>();

    @BeforeEach
    public void setUp() throws IOException {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/sdk", exchange -> {
            try {
                receivedMethod.set(exchange.getRequestMethod());
                receivedContentType.set(exchange.getRequestHeaders().getFirst("Content-Type"));
                receivedTrace.set(exchange.getRequestHeaders().get("X-Trace"));
                receivedBody.set(exchange.getRequestBody().readAllBytes());
                exchange.sendResponseHeaders(204, -1);
            } finally {
                exchange.close();
            }
        });
        server.start();
        RegisterConfig config = new RegisterConfig();
        config.setServerLists("127.0.0.1:" + server.getAddress().getPort());
        config.getProps().setProperty("http.connectTimeout", "5");
        config.getProps().setProperty("http.readTimeout", "5");
        client = new OkHttpShenyuSdkClient();
        client.init(config, Collections.emptyList(), null);
    }

    @AfterEach
    public void tearDown() throws IllegalAccessException {
        try {
            if (Objects.nonNull(client)) {
                OkHttpClient transport = (OkHttpClient) FieldUtils.readField(client, "okHttpClient", true);
                if (Objects.nonNull(transport)) {
                    transport.connectionPool().evictAll();
                    transport.dispatcher().executorService().shutdownNow();
                }
            }
        } finally {
            if (Objects.nonNull(server)) {
                server.stop(0);
            }
        }
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        "POST | Content-Type | text/plain;charset=UTF-8 | café | UTF-8",
        "PUT | content-type | application/xml;charset=UTF-8 | <note>中文</note> | UTF-8",
        "PATCH | CoNtEnT-TyPe | application/x-www-form-urlencoded;charset=UTF-8 | name=shen%20yu | UTF-8",
        "DELETE | Content-Type | text/plain;charset=ISO-8859-1 | café | ISO-8859-1",
        "POST | Content-Type | text/plain;charset=UTF-16LE | café中文 | UTF-16LE",
        "POST | Content-Type | application/problem+json;charset=UTF-8 | {\"title\":\"中文\"} | UTF-8",
        "POST | Content-Type | application/json;charset=ISO-8859-1 | {\"name\":\"café\"} | ISO-8859-1",
        "POST | Content-Type | application/json;charset=UTF-8 | {\"name\":\"中文\"} | UTF-8",
        "POST | Content-Type | text/plain; charset=UTF-8 | café | UTF-8",
        "POST | Content-Type | Text/Plain; charset=\"ISO-8859-1\" | café | ISO-8859-1"
    })
    public void testDeclaredContentTypeAndCharsetReachServer(final ShenyuRequest.HttpMethod method, final String headerName,
                                                           final String contentType, final String body, final String charsetName) throws IOException {
        Map<String, Collection<String>> headers = headers();
        headers.put(headerName, Collections.singletonList(contentType));

        ShenyuResponse response = client.execute(request(method, headers, body));

        assertAll(
                () -> assertEquals(204, response.getStatus()),
                () -> assertEquals(method.name(), receivedMethod.get()),
                () -> assertContentType(contentType),
                () -> assertArrayEquals(body.getBytes(Charset.forName(charsetName)), receivedBody.get()),
                () -> assertEquals(Arrays.asList("first", "second"), receivedTrace.get()));
    }

    @Test
    public void testMissingContentTypeKeepsJsonUtf8Default() throws IOException {
        String body = "{\"name\":\"中文\"}";

        client.execute(request(ShenyuRequest.HttpMethod.POST, headers(), body));

        assertContentType("application/json;charset=UTF-8");
        assertArrayEquals(body.getBytes(StandardCharsets.UTF_8), receivedBody.get());
    }

    @Test
    public void testContentTypeWithoutCharsetUsesUtf8() throws IOException {
        String body = "café中文";
        Map<String, Collection<String>> headers = headers();
        headers.put("Content-Type", Collections.singletonList("text/plain"));

        client.execute(request(ShenyuRequest.HttpMethod.POST, headers, body));

        assertContentType("text/plain; charset=utf-8");
        assertArrayEquals(body.getBytes(StandardCharsets.UTF_8), receivedBody.get());
    }

    @ParameterizedTest
    @NullAndEmptySource
    public void testEmptyBodyKeepsDeclaredContentType(final String body) throws IOException {
        Map<String, Collection<String>> headers = headers();
        headers.put("content-type", Collections.singletonList("text/plain;charset=ISO-8859-1"));

        client.execute(request(ShenyuRequest.HttpMethod.POST, headers, body));

        assertEquals("text/plain;charset=ISO-8859-1", receivedContentType.get());
        assertArrayEquals(new byte[0], receivedBody.get());
    }

    @ParameterizedTest
    @EnumSource(value = ShenyuRequest.HttpMethod.class, names = {"GET", "HEAD"})
    public void testBodylessMethodsKeepDeclaredContentType(final ShenyuRequest.HttpMethod method) throws IOException {
        Map<String, Collection<String>> headers = headers();
        headers.put("Content-Type", Collections.singletonList("text/plain"));

        client.execute(request(method, headers, null));

        assertEquals(method.name(), receivedMethod.get());
        assertEquals("text/plain", receivedContentType.get());
        assertArrayEquals(new byte[0], receivedBody.get());
    }

    private Map<String, Collection<String>> headers() {
        Map<String, Collection<String>> headers = new LinkedHashMap<>();
        headers.put("X-Trace", Arrays.asList("first", "second"));
        return headers;
    }

    private void assertContentType(final String expected) {
        MediaType expectedType = MediaType.get(expected);
        MediaType actualType = MediaType.get(receivedContentType.get());
        assertAll(
                () -> assertEquals(expectedType.type(), actualType.type()),
                () -> assertEquals(expectedType.subtype(), actualType.subtype()),
                () -> assertEquals(expectedType.charset(), actualType.charset()));
    }

    private ShenyuRequest request(final ShenyuRequest.HttpMethod method, final Map<String, Collection<String>> headers, final String body) {
        return ShenyuRequest.create(method, "http://gateway.example/sdk", headers, body, "test", null);
    }
}
