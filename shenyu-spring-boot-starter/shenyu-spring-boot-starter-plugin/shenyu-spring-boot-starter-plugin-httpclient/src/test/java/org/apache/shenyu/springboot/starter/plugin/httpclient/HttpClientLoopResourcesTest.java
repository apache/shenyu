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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import io.netty.handler.timeout.IdleStateHandler;
import io.netty.handler.timeout.ReadTimeoutHandler;
import io.netty.handler.timeout.WriteTimeoutHandler;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties.Pool.PoolType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class HttpClientLoopResourcesTest {

    @ParameterizedTest
    @EnumSource(PoolType.class)
    void usesSuppliedEventLoopsAndRetainsConnectionHandlers(final PoolType poolType) {
        DisposableServer upstream = upstream();
        LoopResources loops = LoopResources.create("httpclient-custom", 1, 2, true);
        HttpClientProperties properties = new HttpClientProperties();
        properties.getPool().setType(poolType);
        HttpClient client = new HttpClientFactory(properties, loops, new ServerProperties()).createInstance();
        AtomicReference<String> thread = new AtomicReference<>();
        AtomicBoolean handlersInstalled = new AtomicBoolean();
        try {
            HttpClient observed = client.doOnConnected(connection -> {
                thread.set(Thread.currentThread().getName());
                handlersInstalled.set(Objects.nonNull(connection.channel().pipeline().get(IdleStateHandler.class))
                        && Objects.nonNull(connection.channel().pipeline().get(WriteTimeoutHandler.class))
                        && Objects.nonNull(connection.channel().pipeline().get(ReadTimeoutHandler.class)));
            });
            assertEquals("ok", request(observed, upstream));
            assertNotNull(thread.get());
            assertTrue(thread.get().startsWith("httpclient-custom-"), thread::get);
            assertTrue(handlersInstalled.get());
        } finally {
            client.configuration().connectionProvider().disposeLater().block(Duration.ofSeconds(5));
            loops.disposeLater().block(Duration.ofSeconds(5));
            upstream.disposeNow();
        }
    }

    @Test
    void usesThreadPoolPropertiesThroughSpringConfiguration() {
        DisposableServer upstream = upstream();
        try {
            new ApplicationContextRunner()
                    .withConfiguration(AutoConfigurations.of(HttpClientPluginConfiguration.class))
                    .withUserConfiguration(PropertyBindingConfiguration.class)
                    .withBean(ServerProperties.class)
                    .withPropertyValues("shenyu.httpclient.thread-pool.prefix=bound-httpclient",
                            "shenyu.httpclient.thread-pool.worker-count=1", "shenyu.httpclient.thread-pool.select-count=1",
                            "shenyu.httpclient.pool.type=DISABLED")
                    .run(context -> {
                        LoopResources loops = context.getBean("httpClientLoopResource", LoopResources.class);
                        AtomicReference<String> thread = new AtomicReference<>();
                        HttpClient client = context.getBean("httpClient", HttpClient.class)
                                .doOnConnected(connection -> thread.set(Thread.currentThread().getName()));
                        try {
                            assertEquals("ok", request(client, upstream));
                            assertNotNull(thread.get());
                            assertTrue(thread.get().startsWith("bound-httpclient-"), thread::get);
                        } finally {
                            loops.disposeLater().block(Duration.ofSeconds(5));
                        }
                    });
        } finally {
            upstream.disposeNow();
        }
    }

    @Test
    void usesDefaultResourcesWhenNoCustomLoopsAreSupplied() {
        DisposableServer upstream = upstream();
        HttpClientProperties properties = new HttpClientProperties();
        properties.getPool().setType(PoolType.DISABLED);
        HttpClient client = new HttpClientFactory(properties, null, new ServerProperties()).createInstance();
        try {
            assertEquals("ok", request(client, upstream));
        } finally {
            upstream.disposeNow();
        }
    }

    private DisposableServer upstream() {
        return HttpServer.create().host("127.0.0.1").port(0)
                .handle((request, response) -> response.sendString(Mono.just("ok"))).bindNow();
    }

    private String request(final HttpClient client, final DisposableServer upstream) {
        return client.get().uri("http://127.0.0.1:" + upstream.port()).responseSingle((response, body) -> body.asString())
                .block(Duration.ofSeconds(5));
    }

    @Configuration(proxyBeanMethods = false)
    @EnableConfigurationProperties
    static class PropertyBindingConfiguration {
    }
}
