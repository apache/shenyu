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

package org.apache.shenyu.bootstrap;

import io.netty.handler.codec.http.HttpHeaderNames;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.config.YamlPropertiesFactoryBean;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.autoconfigure.web.reactive.WebFluxProperties;
import org.springframework.boot.autoconfigure.web.reactive.WebSessionIdResolverAutoConfiguration;
import org.springframework.boot.context.properties.bind.Binder;
import org.springframework.boot.context.properties.source.MapConfigurationPropertySource;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.ResponseCookie;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.server.adapter.HttpWebHandlerAdapter;
import org.springframework.web.server.session.DefaultWebSessionManager;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;
import reactor.util.function.Tuple2;
import reactor.util.function.Tuples;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises the packaged bootstrap session-cookie configuration over real HTTP.
 */
class GatewaySessionCookieTest {

    @Test
    void testBackendSessionCookieSurvivesGatewaySessionAccess() {
        Tuple2<List<String>, String> response = request(serverProperties(null), "SESSION=backend-session", false);

        assertEquals("backend-session", response.getT2());
        assertTrue(response.getT1().stream().anyMatch(cookie -> cookie.startsWith("SESSION=backend-renewed")));
        assertFalse(response.getT1().stream().anyMatch(cookie -> cookie.startsWith("SESSION=;") && cookie.contains("Max-Age=0")));
    }

    @Test
    void testExpiredGatewaySessionOnlyClearsGatewayCookie() {
        Tuple2<List<String>, String> response = request(serverProperties(null),
                "SHENYU_SESSION=expired-gateway; SESSION=backend-session", false);

        assertEquals("backend-session", response.getT2());
        assertTrue(response.getT1().stream().anyMatch(cookie -> cookie.startsWith("SHENYU_SESSION=;") && cookie.contains("Max-Age=0")));
        assertFalse(response.getT1().stream().anyMatch(cookie -> cookie.startsWith("SESSION=;") && cookie.contains("Max-Age=0")));
    }

    @Test
    void testNewGatewaySessionUsesItsOwnCookie() {
        Tuple2<List<String>, String> response = request(serverProperties(null), "SESSION=backend-session", true);

        assertEquals("backend-session", response.getT2());
        assertTrue(response.getT1().stream().anyMatch(cookie -> cookie.startsWith("SHENYU_SESSION=") && !cookie.contains("Max-Age=0")));
    }

    @Test
    void testExplicitCookieNameOverrideIsHonored() {
        Tuple2<List<String>, String> response = request(serverProperties("CUSTOM_GATEWAY_SESSION"), "SESSION=backend-session", true);

        assertTrue(response.getT1().stream().anyMatch(cookie -> cookie.startsWith("CUSTOM_GATEWAY_SESSION=")));
        assertFalse(response.getT1().stream().anyMatch(cookie -> cookie.startsWith("SHENYU_SESSION=")));
    }

    @Test
    void testLegacySharedNameReproducesBackendSessionDeletion() {
        Tuple2<List<String>, String> response = request(serverProperties("SESSION"), "SESSION=backend-session", false);

        assertTrue(response.getT1().stream().anyMatch(cookie -> cookie.startsWith("SESSION=;") && cookie.contains("Max-Age=0")));
    }

    private ServerProperties serverProperties(final String cookieName) {
        YamlPropertiesFactoryBean yaml = new YamlPropertiesFactoryBean();
        yaml.setResources(new ClassPathResource("application.yml"));
        Properties properties = Objects.requireNonNull(yaml.getObject());
        if (Objects.nonNull(cookieName)) {
            properties.setProperty("server.reactive.session.cookie.name", cookieName);
        }
        return new Binder(new MapConfigurationPropertySource(properties)).bind("server", ServerProperties.class).get();
    }

    private Tuple2<List<String>, String> request(final ServerProperties properties, final String cookies, final boolean startSession) {
        DefaultWebSessionManager sessions = new DefaultWebSessionManager();
        sessions.setSessionIdResolver(new WebSessionIdResolverAutoConfiguration(properties, new WebFluxProperties()).webSessionIdResolver());
        HttpWebHandlerAdapter handler = new HttpWebHandlerAdapter(exchange -> exchange.getSession().flatMap(session -> {
            if (startSession) {
                session.getAttributes().put("gateway-authentication", "active");
            }
            exchange.getResponse().addCookie(ResponseCookie.from("SESSION", "backend-renewed").path("/").build());
            String original = Objects.requireNonNull(exchange.getRequest().getCookies().getFirst("SESSION")).getValue();
            byte[] bytes = original.getBytes(StandardCharsets.UTF_8);
            return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(bytes)));
        }));
        handler.setSessionManager(sessions);
        handler.afterPropertiesSet();
        LoopResources loops = LoopResources.create("session-cookie-test", 1, true);
        DisposableServer server = null;
        try {
            server = HttpServer.create().host("127.0.0.1").port(0).runOn(loops)
                    .handle(new ReactorHttpHandlerAdapter(handler)).bindNow(Duration.ofSeconds(5));
            return Objects.requireNonNull(HttpClient.newConnection().runOn(loops).keepAlive(false)
                    .headers(headers -> headers.set(HttpHeaderNames.COOKIE, cookies)).get()
                    .uri("http://127.0.0.1:" + server.port() + "/session")
                    .responseSingle((response, body) -> body.asString().defaultIfEmpty("")
                            .map(text -> Tuples.of(List.copyOf(response.responseHeaders().getAll(HttpHeaderNames.SET_COOKIE)), text)))
                    .block(Duration.ofSeconds(5)));
        } finally {
            try {
                if (Objects.nonNull(server)) {
                    server.disposeNow(Duration.ofSeconds(5));
                }
            } finally {
                loops.disposeLater().block(Duration.ofSeconds(5));
            }
        }
    }
}
