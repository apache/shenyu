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
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Request charsets and raw body bytes sent by the Apache HTTP SDK transport.
 */
@Timeout(10)
public final class HttpRequestCharsetTest {

    private HttpServer server;

    private HttpShenyuSdkClient client;

    private final AtomicReference<String> receivedMethod = new AtomicReference<>();

    private final AtomicReference<String> receivedContentType = new AtomicReference<>();

    private final AtomicReference<String> receivedContentLength = new AtomicReference<>();

    private final AtomicReference<byte[]> receivedBody = new AtomicReference<>();

    @BeforeEach
    public void setUp() throws IOException {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/sdk", exchange -> {
            try {
                receivedMethod.set(exchange.getRequestMethod());
                receivedContentType.set(exchange.getRequestHeaders().getFirst("Content-Type"));
                receivedContentLength.set(exchange.getRequestHeaders().getFirst("Content-Length"));
                receivedBody.set(exchange.getRequestBody().readAllBytes());
                exchange.sendResponseHeaders(204, -1);
            } finally {
                exchange.close();
            }
        });
        server.start();
        client = new HttpShenyuSdkClient();
        Properties props = new Properties();
        props.setProperty("http.serverRequestTimeOut", "5000");
        props.setProperty("http.serverResponseTimeOut", "5000");
        client.initClient(props);
    }

    @AfterEach
    public void tearDown() throws IOException {
        try {
            if (Objects.nonNull(client)) {
                client.close();
            }
        } finally {
            if (Objects.nonNull(server)) {
                server.stop(0);
            }
        }
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        "POST | Content-Type | text/plain;charset=ISO-8859-1 | café | ISO-8859-1",
        "PUT | content-type | application/xml;charset=UTF-16LE | <value>café中文</value> | UTF-16LE",
        "PATCH | CONTENT-TYPE | text/plain; charset=\"ISO-8859-1\" | café | ISO-8859-1",
        "POST | Content-Type | application/json;charset=ISO-8859-1 | {\"value\":\"café\"} | ISO-8859-1",
        "DELETE | Content-Type | text/plain;charset=UTF-8 | café中文 | UTF-8",
        "POST | Content-Type | text/plain | café中文 | UTF-8",
        "POST | Content-Type | application/json | {\"value\":\"café中文\"} | UTF-8"
    })
    public void testDeclaredCharsetControlsRequestBytes(final ShenyuRequest.HttpMethod method, final String headerName,
                                                       final String contentType, final String body, final String charsetName) throws IOException {
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put(headerName, Collections.singletonList(contentType));

        ShenyuResponse response = client.doRequest(request(method, headers, body));

        final byte[] expectedBody = body.getBytes(Charset.forName(charsetName));
        assertEquals(204, response.getStatus());
        assertEquals(method.name(), receivedMethod.get());
        assertEquals(contentType, receivedContentType.get());
        assertArrayEquals(expectedBody, receivedBody.get());
        assertEquals(Integer.toString(expectedBody.length), receivedContentLength.get());
    }

    @Test
    public void testMissingContentTypeKeepsJsonUtf8Default() throws IOException {
        String body = "{\"value\":\"café中文\"}";

        client.doRequest(request(ShenyuRequest.HttpMethod.POST, Collections.emptyMap(), body));

        final byte[] expectedBody = body.getBytes(StandardCharsets.UTF_8);
        assertEquals("application/json;charset=UTF-8", receivedContentType.get());
        assertArrayEquals(expectedBody, receivedBody.get());
        assertEquals(Integer.toString(expectedBody.length), receivedContentLength.get());
    }

    @ParameterizedTest
    @NullAndEmptySource
    public void testEmptyBodyKeepsDeclaredContentType(final String body) throws IOException {
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("content-type", Collections.singletonList("text/plain;charset=ISO-8859-1"));

        client.doRequest(request(ShenyuRequest.HttpMethod.POST, headers, body));

        assertEquals("text/plain;charset=ISO-8859-1", receivedContentType.get());
        assertArrayEquals(new byte[0], receivedBody.get());
        assertNull(receivedContentLength.get());
    }

    private ShenyuRequest request(final ShenyuRequest.HttpMethod method, final Map<String, Collection<String>> headers, final String body) {
        String url = "http://127.0.0.1:" + server.getAddress().getPort() + "/sdk";
        return ShenyuRequest.create(method, url, headers, body, null, null);
    }
}
