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

package org.apache.shenyu.springboot.starter.netty;

import io.netty.handler.ssl.util.InsecureTrustManagerFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.server.GracefulShutdownResult;
import org.springframework.boot.web.server.Shutdown;
import org.springframework.boot.web.server.Ssl;
import org.springframework.boot.web.server.WebServer;
import org.springframework.boot.web.server.WebServerException;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.server.reactive.HttpHandler;
import org.springframework.util.ResourceUtils;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.netty.DisposableServer;
import reactor.netty.http.Http11SslContextSpec;
import reactor.netty.http.client.HttpClient;
import reactor.netty.tcp.SslProvider;

import javax.net.ssl.SNIHostName;

import java.net.InetAddress;
import java.net.ServerSocket;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Real HTTP and TLS requests exercise the shared handler and listener lifecycle.
 */
class ShenyuDualProtocolWebServerTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuNettyWebServerConfiguration.class))
            .withUserConfiguration(PropertiesConfiguration.class)
            .withPropertyValues("shenyu.netty.http.worker-count=2");

    @Test
    void testDefaultHttpListener() {
        runner.run(context -> {
            NettyReactiveWebServerFactory factory = context.getBean(NettyReactiveWebServerFactory.class);
            factory.setPort(0);
            List<DisposableServer> listeners = captureListeners(factory);
            WebServer server = factory.getWebServer(echoHandler());
            try {
                server.start();
                assertEquals("http:/gateway", request(false, server.getPort()).block(Duration.ofSeconds(5)));
                assertEquals(1, listeners.size());
            } finally {
                server.stop();
            }
            assertTrue(listeners.stream().allMatch(DisposableServer::isDisposed));
        });
    }

    @Test
    void testHttpAndBootHttpsShareHandler() {
        runner.withPropertyValues("shenyu.netty.http.additional-http-port=0").run(context -> {
            NettyReactiveWebServerFactory factory = context.getBean(NettyReactiveWebServerFactory.class);
            factory.setPort(0);
            configureSsl(factory);
            verifyBothProtocols(factory);
        });
    }

    @Test
    void testHttpAlongsideSniHttps() throws Exception {
        runner.withPropertyValues(
                "shenyu.netty.http.additional-http-port=0",
                "shenyu.netty.http.web-server-factory-enabled=true",
                "shenyu.netty.http.sni.enabled=true",
                "shenyu.netty.http.sni.mod=manual",
                "shenyu.netty.http.sni.certificates[0].domain=localhost",
                "shenyu.netty.http.sni.certificates[0].key-cert-chain-file=" + ResourceUtils.getFile("classpath:ssl/localhost-cert.pem"),
                "shenyu.netty.http.sni.certificates[0].key-file=" + ResourceUtils.getFile("classpath:ssl/localhost-key.pem"))
                .run(context -> {
                    NettyReactiveWebServerFactory factory = context.getBean(NettyReactiveWebServerFactory.class);
                    factory.setPort(0);
                    verifyBothProtocols(factory, true);
                });
    }

    @Test
    void testHttpsOnlyRemainsDefault() {
        runner.run(context -> {
            NettyReactiveWebServerFactory factory = context.getBean(NettyReactiveWebServerFactory.class);
            factory.setPort(0);
            configureSsl(factory);
            List<DisposableServer> listeners = captureListeners(factory);
            WebServer server = factory.getWebServer(echoHandler());
            try {
                server.start();
                assertEquals("https:/gateway", request(true, server.getPort()).block(Duration.ofSeconds(5)));
                assertEquals(1, listeners.size());
            } finally {
                server.stop();
            }
            assertTrue(listeners.stream().allMatch(DisposableServer::isDisposed));
        });
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, 65536})
    void testInvalidAdditionalPort(final int port) {
        runner.withPropertyValues("shenyu.netty.http.additional-http-port=" + port).run(context -> {
            NettyReactiveWebServerFactory factory = context.getBean(NettyReactiveWebServerFactory.class);
            IllegalArgumentException error = assertThrows(IllegalArgumentException.class, () -> factory.getWebServer(echoHandler()));
            assertTrue(error.getMessage().contains("between 0 and 65535"));
        });
    }

    @Test
    void testAdditionalPortRequiresHttps() {
        runner.withPropertyValues("shenyu.netty.http.additional-http-port=0").run(context -> {
            NettyReactiveWebServerFactory factory = context.getBean(NettyReactiveWebServerFactory.class);
            IllegalArgumentException error = assertThrows(IllegalArgumentException.class, () -> factory.getWebServer(echoHandler()));
            assertTrue(error.getMessage().contains("requires server.ssl or SNI HTTPS"));
        });
    }

    @Test
    void testAdditionalPortMustDifferFromPrimary() {
        runner.withPropertyValues("shenyu.netty.http.additional-http-port=9195").run(context -> {
            NettyReactiveWebServerFactory factory = context.getBean(NettyReactiveWebServerFactory.class);
            factory.setPort(9195);
            assertThrows(IllegalArgumentException.class, () -> factory.getWebServer(echoHandler()));
        });
    }

    @Test
    void testAdditionalBindFailureStopsPrimary() throws Exception {
        try (ServerSocket occupied = new ServerSocket(0, 1, InetAddress.getLoopbackAddress())) {
            runner.withPropertyValues("shenyu.netty.http.additional-http-port=" + occupied.getLocalPort()).run(context -> {
                NettyReactiveWebServerFactory factory = context.getBean(NettyReactiveWebServerFactory.class);
                factory.setPort(0);
                factory.setAddress(InetAddress.getLoopbackAddress());
                configureSsl(factory);
                List<DisposableServer> listeners = captureListeners(factory);
                WebServer server = factory.getWebServer(echoHandler());
                try {
                    assertThrows(WebServerException.class, server::start);
                    assertEquals(1, listeners.size());
                    assertTrue(listeners.get(0).isDisposed());
                } finally {
                    server.stop();
                }
            });
        }
    }

    @Test
    void testGracefulShutdownWaitsForBothListeners() {
        runner.withPropertyValues("shenyu.netty.http.additional-http-port=0").run(context -> {
            NettyReactiveWebServerFactory factory = context.getBean(NettyReactiveWebServerFactory.class);
            factory.setPort(0);
            factory.setShutdown(Shutdown.GRACEFUL);
            configureSsl(factory);
            List<DisposableServer> listeners = captureListeners(factory);
            CountDownLatch entered = new CountDownLatch(2);
            Sinks.One<Void> http = Sinks.one();
            Sinks.One<Void> https = Sinks.one();
            WebServer server = factory.getWebServer((request, response) -> {
                entered.countDown();
                return ("https".equals(request.getURI().getScheme()) ? https : http).asMono().then(response.setComplete());
            });
            CompletableFuture<GracefulShutdownResult> shutdown = new CompletableFuture<>();
            try {
                server.start();
                final CompletableFuture<String> tlsResponse = request(true, server.getPort()).toFuture();
                final CompletableFuture<String> plainResponse = request(false, listeners.get(1).port()).toFuture();
                assertTrue(entered.await(5, TimeUnit.SECONDS));
                server.shutDownGracefully(shutdown::complete);
                assertFalse(shutdown.isDone());
                https.tryEmitEmpty();
                tlsResponse.get(5, TimeUnit.SECONDS);
                assertFalse(shutdown.isDone());
                http.tryEmitEmpty();
                plainResponse.get(5, TimeUnit.SECONDS);
                assertEquals(GracefulShutdownResult.IDLE, shutdown.get(5, TimeUnit.SECONDS));
            } finally {
                http.tryEmitEmpty();
                https.tryEmitEmpty();
                server.stop();
            }
        });
    }

    private void verifyBothProtocols(final NettyReactiveWebServerFactory factory) {
        verifyBothProtocols(factory, false);
    }

    private void verifyBothProtocols(final NettyReactiveWebServerFactory factory, final boolean sni) {
        factory.addRouteProviders(routes -> routes.get("/probe", (request, response) -> response.sendString(Mono.just("route-provider"))));
        List<DisposableServer> listeners = captureListeners(factory);
        WebServer server = factory.getWebServer(echoHandler());
        try {
            server.start();
            assertEquals(2, listeners.size());
            assertEquals(listeners.get(0).port(), server.getPort());
            assertEquals("https:/gateway", request(true, server.getPort(), "/gateway", sni).block(Duration.ofSeconds(5)));
            assertEquals("http:/gateway", request(false, listeners.get(1).port()).block(Duration.ofSeconds(5)));
            assertEquals("route-provider", request(true, server.getPort(), "/probe", sni).block(Duration.ofSeconds(5)));
            assertEquals("route-provider", request(false, listeners.get(1).port(), "/probe").block(Duration.ofSeconds(5)));
        } finally {
            server.stop();
        }
        assertTrue(listeners.stream().allMatch(DisposableServer::isDisposed));
    }

    private List<DisposableServer> captureListeners(final NettyReactiveWebServerFactory factory) {
        List<DisposableServer> listeners = new CopyOnWriteArrayList<>();
        factory.addServerCustomizers(server -> server.doOnBound(listeners::add));
        return listeners;
    }

    private void configureSsl(final NettyReactiveWebServerFactory factory) {
        Ssl ssl = new Ssl();
        ssl.setCertificate("classpath:ssl/localhost-cert.pem");
        ssl.setCertificatePrivateKey("classpath:ssl/localhost-key.pem");
        factory.setSsl(ssl);
    }

    private HttpHandler echoHandler() {
        return (request, response) -> {
            response.getHeaders().set("X-Gateway-Handler", "shared");
            byte[] bytes = (request.getURI().getScheme() + ":" + request.getURI().getPath()).getBytes(StandardCharsets.UTF_8);
            return response.writeWith(Mono.just(response.bufferFactory().wrap(bytes)));
        };
    }

    private Mono<String> request(final boolean secure, final int port) {
        return request(secure, port, "/gateway");
    }

    private Mono<String> request(final boolean secure, final int port, final String path) {
        return request(secure, port, path, false);
    }

    private Mono<String> request(final boolean secure, final int port, final String path, final boolean sni) {
        HttpClient client = HttpClient.newConnection().responseTimeout(Duration.ofSeconds(5));
        if (secure) {
            client = client.secure(spec -> {
                SslProvider.Builder ssl = spec.sslContext(Http11SslContextSpec.forClient()
                        .configure(builder -> builder.trustManager(InsecureTrustManagerFactory.INSTANCE)));
                if (sni) {
                    ssl.serverNames(new SNIHostName("localhost"));
                }
            });
        }
        return client.get().uri((secure ? "https" : "http") + "://localhost:" + port + path)
                .responseSingle((response, body) -> {
                    assertEquals(200, response.status().code());
                    return body.asString().defaultIfEmpty("");
                });
    }

    @Configuration(proxyBeanMethods = false)
    @EnableConfigurationProperties
    static class PropertiesConfiguration {
    }
}
