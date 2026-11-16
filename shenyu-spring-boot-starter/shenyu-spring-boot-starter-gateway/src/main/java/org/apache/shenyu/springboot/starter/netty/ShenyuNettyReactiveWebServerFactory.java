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

import org.apache.shenyu.common.config.NettyHttpProperties;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.embedded.netty.NettyRouteProvider;
import org.springframework.boot.web.embedded.netty.NettyServerCustomizer;
import org.springframework.boot.web.embedded.netty.NettyWebServer;
import org.springframework.boot.web.server.GracefulShutdownCallback;
import org.springframework.boot.web.server.GracefulShutdownResult;
import org.springframework.boot.web.server.WebServer;
import org.springframework.http.client.ReactorResourceFactory;
import org.springframework.http.server.reactive.HttpHandler;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.util.Assert;
import reactor.netty.http.HttpProtocol;
import reactor.netty.http.server.HttpServer;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Netty factory supporting an optional HTTP listener alongside HTTPS.
 */
final class ShenyuNettyReactiveWebServerFactory extends NettyReactiveWebServerFactory {

    private final NettyHttpProperties properties;

    private final List<NettyRouteProvider> routeProviders = new ArrayList<>();

    private Duration lifecycleTimeout;

    private ReactorResourceFactory resourceFactory;

    ShenyuNettyReactiveWebServerFactory(final NettyHttpProperties properties) {
        this.properties = properties;
    }

    @Override
    public void setLifecycleTimeout(final Duration lifecycleTimeout) {
        super.setLifecycleTimeout(lifecycleTimeout);
        this.lifecycleTimeout = lifecycleTimeout;
    }

    @Override
    public void setResourceFactory(final ReactorResourceFactory resourceFactory) {
        super.setResourceFactory(resourceFactory);
        this.resourceFactory = resourceFactory;
    }

    @Override
    public void addRouteProviders(final NettyRouteProvider... routeProviders) {
        super.addRouteProviders(routeProviders);
        this.routeProviders.addAll(Arrays.asList(routeProviders));
    }

    @Override
    public WebServer getWebServer(final HttpHandler httpHandler) {
        Integer httpPort = properties.getAdditionalHttpPort();
        if (Objects.isNull(httpPort)) {
            return super.getWebServer(httpHandler);
        }
        Assert.isTrue(httpPort >= 0 && httpPort <= 65535, "additionalHttpPort must be between 0 and 65535");
        Assert.isTrue(httpPort == 0 || httpPort != getPort(), "additionalHttpPort must differ from server.port");

        // Capture Boot's fully customized server so both listeners use the same handler,
        // address, resources, decoder, compression and forwarding configuration.
        List<NettyServerCustomizer> customizers = new ArrayList<>(getServerCustomizers());
        AtomicReference<HttpServer> configuredServer = new AtomicReference<>();
        addServerCustomizers(server -> {
            Assert.isTrue(server.configuration().isSecure(), "additionalHttpPort requires server.ssl or SNI HTTPS");
            configuredServer.set(server);
            return server;
        });
        WebServer primary;
        try {
            primary = super.getWebServer(httpHandler);
        } finally {
            setServerCustomizers(customizers);
        }
        HttpServer server = configuredServer.get();
        NettyWebServer additional = new NettyWebServer(server.noSSL().protocol(HttpProtocol.HTTP11).port(httpPort),
                new ReactorHttpHandlerAdapter(httpHandler), lifecycleTimeout, getShutdown(), resourceFactory);
        additional.setRouteProviders(routeProviders);
        return new DualProtocolWebServer(primary, additional);
    }

    private static final class DualProtocolWebServer implements WebServer {

        private final WebServer primary;

        private final WebServer additional;

        private DualProtocolWebServer(final WebServer primary, final WebServer additional) {
            this.primary = primary;
            this.additional = additional;
        }

        @Override
        public void start() {
            try {
                primary.start();
                additional.start();
            } catch (RuntimeException ex) {
                try {
                    stop();
                } catch (RuntimeException stopFailure) {
                    ex.addSuppressed(stopFailure);
                }
                throw ex;
            }
        }

        @Override
        public void stop() {
            try {
                additional.stop();
            } finally {
                primary.stop();
            }
        }

        @Override
        public int getPort() {
            return primary.getPort();
        }

        @Override
        public void shutDownGracefully(final GracefulShutdownCallback callback) {
            AtomicInteger remaining = new AtomicInteger(2);
            AtomicBoolean idle = new AtomicBoolean(true);
            GracefulShutdownCallback completion = result -> {
                if (result != GracefulShutdownResult.IDLE) {
                    idle.set(false);
                }
                if (remaining.decrementAndGet() == 0) {
                    callback.shutdownComplete(idle.get() ? GracefulShutdownResult.IDLE : GracefulShutdownResult.REQUESTS_ACTIVE);
                }
            };
            primary.shutDownGracefully(completion);
            additional.shutDownGracefully(completion);
        }

        @Override
        public void destroy() {
            try {
                additional.destroy();
            } finally {
                primary.destroy();
            }
        }
    }
}
