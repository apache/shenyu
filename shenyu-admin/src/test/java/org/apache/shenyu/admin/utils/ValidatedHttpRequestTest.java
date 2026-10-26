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
import okhttp3.Dns;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import org.apache.shenyu.admin.config.properties.SwaggerImportProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.net.SocketFactory;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Exercise real HTTP transport without contacting an external server.
 */
public class ValidatedHttpRequestTest {

    private HttpServer server;

    private OkHttpClient baseClient;

    private HttpUtils httpUtils;

    private InetAddress publicAddress;

    private final List<InetAddress> connectedAddresses = new ArrayList<>();

    private final AtomicInteger privateRequests = new AtomicInteger();

    private final AtomicReference<String> hostHeader = new AtomicReference<>();

    @BeforeEach
    public void setUp() throws IOException {
        publicAddress = InetAddress.getByName("8.8.8.8");
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/docs", exchange -> {
            hostHeader.set(exchange.getRequestHeaders().getFirst("Host"));
            byte[] body = "swagger document".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.createContext("/redirect", exchange -> {
            exchange.getResponseHeaders().add("Location", "http://127.0.0.1:" + server.getAddress().getPort() + "/private");
            exchange.sendResponseHeaders(302, -1);
            exchange.close();
        });
        server.createContext("/private", exchange -> {
            privateRequests.incrementAndGet();
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });
        server.start();
        SocketFactory socketFactory = mock(SocketFactory.class);
        when(socketFactory.createSocket()).thenAnswer(invocation -> new Socket() {
            @Override
            public void connect(final SocketAddress endpoint, final int timeout) throws IOException {
                connectedAddresses.add(((InetSocketAddress) endpoint).getAddress());
                // Record OkHttp's actual selected destination, then route only this test socket to the fixture.
                super.connect(server.getAddress(), timeout);
            }
        });
        baseClient = new OkHttpClient.Builder().socketFactory(socketFactory)
                .dns(host -> Collections.singletonList(InetAddress.getByName("127.0.0.1")))
                .proxy(Proxy.NO_PROXY).build();
        httpUtils = new HttpUtils();
        httpUtils.setHttpClient(baseClient);
    }

    @AfterEach
    public void tearDown() {
        baseClient.connectionPool().evictAll();
        server.stop(0);
    }

    @Test
    public void shouldUsePinnedAddressesWithoutRepeatingDnsOrReusingUnvalidatedConnections() throws IOException {
        String url = url("/docs");
        // Populate the ordinary client's pool with a connection to an internal DNS answer.
        try (Response response = baseClient.newCall(new Request.Builder().url(url).build()).execute()) {
            assertEquals("swagger document", response.body().string());
        }
        AtomicInteger resolutions = new AtomicInteger();
        Dns changingDns = host -> Collections.singletonList(resolutions.getAndIncrement() == 0
                ? publicAddress : InetAddress.getByName("127.0.0.1"));
        Dns validatedDns = UrlSecurityUtils.validatedDns(url, new SwaggerImportProperties(), changingDns);
        try (Response response = httpUtils.requestForValidatedResponse(url, validatedDns)) {
            assertEquals(200, response.code());
            assertEquals("swagger document", response.body().string());
        }
        assertEquals(1, resolutions.get());
        assertEquals(2, connectedAddresses.size());
        assertEquals(publicAddress, connectedAddresses.get(1));
        assertEquals("docs.example:" + server.getAddress().getPort(), hostHeader.get());
    }

    @Test
    public void shouldBypassInheritedProxyAndNotFollowRedirects() throws IOException {
        httpUtils.setHttpClient(baseClient.newBuilder()
                .proxy(new Proxy(Proxy.Type.HTTP, new InetSocketAddress("127.0.0.1", 1))).build());
        String url = url("/redirect");
        Dns validatedDns = UrlSecurityUtils.validatedDns(url, new SwaggerImportProperties(), host -> Collections.singletonList(publicAddress));
        try (Response response = httpUtils.requestForValidatedResponse(url, validatedDns)) {
            assertEquals(302, response.code());
        }
        assertEquals(0, privateRequests.get());
        assertEquals(Collections.singletonList(publicAddress), connectedAddresses);
    }

    private String url(final String path) {
        return "http://docs.example:" + server.getAddress().getPort() + path;
    }
}
