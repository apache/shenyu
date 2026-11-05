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

package org.apache.shenyu.springboot.starter.plugin.ai.proxy;

import io.netty.channel.ChannelOption;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransportProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.transport.ProxyProvider;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiProxyHttpClientResourcesTest {

    @Test
    void appliesConnectResponseHeaderProxyAndTlsSettings() {
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setConnectTimeout(Duration.ofMillis(1234));
        properties.setResponseHeaderTimeout(Duration.ofMillis(2345));
        properties.getTls().setVerify(false);
        properties.getProxy().setEnabled(true);
        properties.getProxy().setHost("127.0.0.1");
        properties.getProxy().setPort(8080);
        properties.getPool().setName("ai-resource-settings-test");
        final AiProxyHttpClientResources resources = new AiProxyHttpClientResources(properties);
        try {
            assertEquals(1234, resources.getHttpClient().configuration()
                    .options().get(ChannelOption.CONNECT_TIMEOUT_MILLIS));
            assertEquals(Duration.ofMillis(2345),
                    resources.getHttpClient().configuration().responseTimeout());
            assertNotNull(resources.getHttpClient().configuration().sslProvider());
            final ProxyProvider proxy = resources.getHttpClient().configuration().proxyProvider();
            assertNotNull(proxy);
            assertEquals(ProxyProvider.Proxy.HTTP, proxy.getType());
            assertEquals("127.0.0.1", proxy.getAddress().get().getHostString());
            assertEquals(8080, proxy.getAddress().get().getPort());
        } finally {
            resources.close();
        }
    }

    @Test
    void reusesHttpClientAndDisposesOwnedProvider() {
        final DisposableServer server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> response
                        .keepAlive(true)
                        .header(HttpHeaders.CONTENT_LENGTH, "2")
                        .sendString(Mono.just("ok")))
                .bindNow();
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.getPool().setName("ai-resource-reuse-test");
        final AiProxyHttpClientResources resources = new AiProxyHttpClientResources(properties);
        try {
            assertSame(resources.getHttpClient(), resources.getHttpClient());
            assertEquals("ok", get(resources, server));
            assertFalse(resources.getHttpClient().configuration().connectionProvider().isDisposed());
            assertEquals("ok", get(resources, server));
        } finally {
            resources.close();
            server.disposeNow();
        }
        assertTrue(resources.getHttpClient().configuration().connectionProvider().isDisposed());
    }

    @Test
    void classifiesRealConnectionPoolAcquireTimeout() {
        final DisposableServer server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> response.sendString(
                        Flux.just("held").concatWith(Flux.never())))
                .bindNow();
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.getPool().setName("ai-pool-timeout-test");
        properties.getPool().setMaxConnections(1);
        properties.getPool().setPendingAcquireMaxCount(1);
        properties.getPool().setPendingAcquireTimeout(Duration.ofMillis(100));
        properties.setResponseHeaderTimeout(Duration.ofSeconds(1));
        final AiProxyHttpClientResources resources = new AiProxyHttpClientResources(properties);
        final WebClient webClient = WebClient.builder()
                .clientConnector(new ReactorClientHttpConnector(resources.getHttpClient()))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient, properties);
        final AiUpstreamRequest request = request(server);
        Disposable heldRequest = null;
        try {
            final AiUpstreamResponse heldResponse = transport.execute(request).block(Duration.ofSeconds(5));
            heldRequest = heldResponse.getBody().subscribe();

            final AiTransportException error = assertThrows(AiTransportException.class,
                    () -> transport.execute(request).block(Duration.ofSeconds(5)));
            assertEquals(AiTransportException.Kind.CONNECTION_POOL_TIMEOUT, error.getKind());
        } finally {
            if (Objects.nonNull(heldRequest)) {
                heldRequest.dispose();
            }
            resources.close();
            server.disposeNow();
        }
    }

    private String get(final AiProxyHttpClientResources resources, final DisposableServer server) {
        return resources.getHttpClient()
                .get()
                .uri("http://127.0.0.1:" + server.port())
                .responseSingle((response, body) -> body.asString())
                .block(Duration.ofSeconds(5));
    }

    private AiUpstreamRequest request(final DisposableServer server) {
        final AiUpstreamRequest request = new AiUpstreamRequest();
        request.setMethod(HttpMethod.GET.name());
        request.setUri(URI.create("http://127.0.0.1:" + server.port()));
        request.setHeaders(Map.of());
        request.setBody("".getBytes(StandardCharsets.UTF_8));
        return request;
    }
}
