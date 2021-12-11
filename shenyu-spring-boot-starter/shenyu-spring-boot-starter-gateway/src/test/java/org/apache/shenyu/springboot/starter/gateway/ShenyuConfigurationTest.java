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

package org.apache.shenyu.springboot.starter.gateway;

import io.netty.channel.ChannelOption;
import io.netty.channel.WriteBufferWaterMark;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.springboot.starter.netty.NettyTcpProperties;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.embedded.netty.NettyServerCustomizer;
import org.springframework.boot.web.reactive.error.DefaultErrorAttributes;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.support.DefaultServerCodecConfigurer;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.WebFilter;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

import java.util.Optional;

import static org.junit.Assert.assertNotNull;

/**
 * Test case for {@link ShenyuConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class ShenyuConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @Before
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuConfiguration.class))
            .withBean(ShenyuConfigurationTest.class)
            .withBean(DefaultServerCodecConfigurer.class)
            .withBean(DefaultErrorAttributes.class)
            .withPropertyValues(
                "debug=true",
                "shenyu.cross.enabled=true",
                "shenyu.file.enabled=true",
                "shenyu.exclude.enabled=true"
            );
    }

    @Test
    public void testShenyuWebHandler() {
        applicationContextRunner.run(context -> {
                ShenyuWebHandler handler = context.getBean("webHandler", ShenyuWebHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testDispatcherHandler() {
        applicationContextRunner.run(context -> {
                DispatcherHandler handler = context.getBean("dispatcherHandler", DispatcherHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testRpcParamTransformPlugin() {
        applicationContextRunner.run(context -> {
                ShenyuPlugin plugin = context.getBean("paramTransformPlugin", ShenyuPlugin.class);
                assertNotNull(plugin);
            }
        );
    }

    @Test
    public void testCommonPluginDataSubscriber() {
        applicationContextRunner.run(context -> {
                PluginDataSubscriber subscriber = context.getBean("pluginDataSubscriber", PluginDataSubscriber.class);
                assertNotNull(subscriber);
            }
        );
    }

    @Test
    public void testShenyuLoaderService() {
        applicationContextRunner.run(context -> {
                ShenyuLoaderService service = context.getBean("shenyuLoaderService", ShenyuLoaderService.class);
                assertNotNull(service);
            }
        );
    }

    @Test
    public void testForwardedRemoteAddressResolver() {
        applicationContextRunner.run(context -> {
                RemoteAddressResolver resolver = context.getBean("remoteAddressResolver", RemoteAddressResolver.class);
                assertNotNull(resolver);
            }
        );
    }

    @Test
    public void testWebFilter() {
        applicationContextRunner.run(context -> {
                WebFilter localDispatcherFilter = context.getBean("localDispatcherFilter", WebFilter.class);
                assertNotNull(localDispatcherFilter);
                WebFilter crossFilter = context.getBean("crossFilter", WebFilter.class);
                assertNotNull(crossFilter);
                WebFilter fileSizeFilter = context.getBean("fileSizeFilter", WebFilter.class);
                assertNotNull(fileSizeFilter);
                WebFilter excludeFilter = context.getBean("excludeFilter", WebFilter.class);
                assertNotNull(excludeFilter);
            }
        );
    }

    @Test
    public void testShenyuConfig() {
        applicationContextRunner.run(context -> {
                ShenyuConfig config = context.getBean("shenyuConfig", ShenyuConfig.class);
                assertNotNull(config);
            }
        );
    }

    @Test
    public void testNettyReactiveWebServerFactory() {
        applicationContextRunner.run(context -> {
            NettyReactiveWebServerFactory config = context.getBean("nettyReactiveWebServerFactory", NettyReactiveWebServerFactory.class);
            assertNotNull(config);
        });
    }

    @Configuration
    static class CustomNettyConfig {
        /**
         * Netty reactive web server factory netty reactive web server factory.
         *
         * @param properties the properties
         * @return the netty reactive web server factory
         */
        @Bean
        public NettyReactiveWebServerFactory nettyReactiveWebServerFactory(final ObjectProvider<NettyTcpProperties> properties) {
            NettyReactiveWebServerFactory webServerFactory = new NettyReactiveWebServerFactory();
            NettyTcpProperties nettyTcpProperties = Optional.ofNullable(properties.getIfAvailable()).orElse(new NettyTcpProperties());
            webServerFactory.addServerCustomizers(new EventLoopNettyCustomizer(nettyTcpProperties));
            return webServerFactory;
        }

        private static class EventLoopNettyCustomizer implements NettyServerCustomizer {

            private final NettyTcpProperties nettyTcpProperties;

            /**
             * Instantiates a new Event loop netty customizer.
             *
             * @param nettyTcpProperties the netty tcp config
             */
            EventLoopNettyCustomizer(final NettyTcpProperties nettyTcpProperties) {
                this.nettyTcpProperties = nettyTcpProperties;
            }

            @Override
            public HttpServer apply(final HttpServer httpServer) {
                return httpServer
                        .tcpConfiguration(tcpServer -> tcpServer
                                .runOn(LoopResources.create("shenyu-netty", nettyTcpProperties.getSelectCount(), nettyTcpProperties.getWorkerCount(), true))
                                .selectorOption(ChannelOption.SO_BACKLOG, nettyTcpProperties.getServerSocketChannelConfig().getSoBacklog())
                                .selectorOption(ChannelOption.SO_REUSEADDR, nettyTcpProperties.getServerSocketChannelConfig().isSoReuseaddr())
                                .selectorOption(ChannelOption.SO_RCVBUF, nettyTcpProperties.getServerSocketChannelConfig().getSoRcvbuf())
                                .selectorOption(ChannelOption.CONNECT_TIMEOUT_MILLIS, nettyTcpProperties.getServerSocketChannelConfig().getConnectTimeoutMillis())
                                .selectorOption(ChannelOption.WRITE_BUFFER_WATER_MARK, new WriteBufferWaterMark(nettyTcpProperties.getServerSocketChannelConfig().getWriteBufferLowWaterMark(),
                                        nettyTcpProperties.getServerSocketChannelConfig().getWriteBufferHighWaterMark()))
                                .selectorOption(ChannelOption.WRITE_SPIN_COUNT, nettyTcpProperties.getServerSocketChannelConfig().getWriteSpinCount())
                                .selectorOption(ChannelOption.AUTO_READ, nettyTcpProperties.getServerSocketChannelConfig().isAutoRead())
                                .selectorOption(ChannelOption.ALLOCATOR, nettyTcpProperties.getServerSocketChannelConfig().getAllocator())

                                .option(ChannelOption.SO_KEEPALIVE, nettyTcpProperties.getSocketChannelConfig().isSoKeepalive())
                                .option(ChannelOption.SO_REUSEADDR, nettyTcpProperties.getSocketChannelConfig().isSoReuseaddr())
                                .option(ChannelOption.SO_LINGER, nettyTcpProperties.getSocketChannelConfig().getSoLinger())
                                .option(ChannelOption.TCP_NODELAY, nettyTcpProperties.getSocketChannelConfig().isTcpNodelay())
                                .option(ChannelOption.SO_RCVBUF, nettyTcpProperties.getSocketChannelConfig().getSoRcvbuf())
                                .option(ChannelOption.SO_SNDBUF, nettyTcpProperties.getSocketChannelConfig().getSoSndbuf())
                                .option(ChannelOption.IP_TOS, nettyTcpProperties.getSocketChannelConfig().getIpTos())
                                .option(ChannelOption.ALLOW_HALF_CLOSURE, nettyTcpProperties.getSocketChannelConfig().isAllowHalfClosure())
                                .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, nettyTcpProperties.getSocketChannelConfig().getConnectTimeoutMillis())
                                .option(ChannelOption.WRITE_BUFFER_WATER_MARK, new WriteBufferWaterMark(nettyTcpProperties.getSocketChannelConfig().getWriteBufferLowWaterMark(),
                                        nettyTcpProperties.getSocketChannelConfig().getWriteBufferHighWaterMark()))
                                .option(ChannelOption.WRITE_SPIN_COUNT, nettyTcpProperties.getSocketChannelConfig().getWriteSpinCount())
                                .option(ChannelOption.AUTO_READ, nettyTcpProperties.getSocketChannelConfig().isAutoRead())
                                .option(ChannelOption.ALLOCATOR, nettyTcpProperties.getSocketChannelConfig().getAllocator()));
            }
        }
    }
}
