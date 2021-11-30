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

import io.netty.channel.ChannelOption;
import io.netty.channel.WriteBufferWaterMark;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.embedded.netty.NettyServerCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

import java.util.Optional;

/**
 * The type shenyu netty web server factory.
 */
@Configuration
public class ShenyuNettyWebServerConfiguration {
    
    /**
     * Netty tcp config.
     *
     * @return the netty tcp config
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.netty.tcp")
    public NettyTcpConfigurationProperties nettyTcpConfigurationProperties() {
        return new NettyTcpConfigurationProperties();
    }
    
    /**
     * Netty reactive web server factory netty reactive web server factory.
     *
     * @param config the config
     * @return the netty reactive web server factory
     */
    @Bean
    public NettyReactiveWebServerFactory nettyReactiveWebServerFactory(final ObjectProvider<NettyTcpConfigurationProperties> config) {
        NettyReactiveWebServerFactory webServerFactory = new NettyReactiveWebServerFactory();
        NettyTcpConfigurationProperties nettyTcpConfigurationProperties = Optional.ofNullable(config.getIfAvailable()).orElse(new NettyTcpConfigurationProperties());
        webServerFactory.addServerCustomizers(new EventLoopNettyCustomizer(nettyTcpConfigurationProperties));
        return webServerFactory;
    }

    private static class EventLoopNettyCustomizer implements NettyServerCustomizer {

        private final NettyTcpConfigurationProperties nettyTcpConfigurationProperties;
    
        /**
         * Instantiates a new Event loop netty customizer.
         *
         * @param nettyTcpConfigurationProperties the netty tcp config
         */
        EventLoopNettyCustomizer(final NettyTcpConfigurationProperties nettyTcpConfigurationProperties) {
            this.nettyTcpConfigurationProperties = nettyTcpConfigurationProperties;
        }

        @Override
        public HttpServer apply(final HttpServer httpServer) {
            return httpServer
                    .tcpConfiguration(tcpServer -> tcpServer
                            .runOn(LoopResources.create("shenyu-netty", nettyTcpConfigurationProperties.getSelectCount(), nettyTcpConfigurationProperties.getWorkerCount(), true))
                            .selectorOption(ChannelOption.SO_BACKLOG, nettyTcpConfigurationProperties.getSoBacklog())
                            .selectorOption(ChannelOption.SO_REUSEADDR, nettyTcpConfigurationProperties.isSoReuseaddr())
                            .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, nettyTcpConfigurationProperties.getConnectTimeoutMillis())
                            .option(ChannelOption.WRITE_BUFFER_WATER_MARK, new WriteBufferWaterMark(nettyTcpConfigurationProperties.getWriteBufferLowWaterMark(),
                                    nettyTcpConfigurationProperties.getWriteBufferHighWaterMark()))
                            .option(ChannelOption.WRITE_SPIN_COUNT, nettyTcpConfigurationProperties.getWriteSpinCount())
                            .option(ChannelOption.AUTO_READ, nettyTcpConfigurationProperties.isAutoRead())
                            .option(ChannelOption.TCP_NODELAY, nettyTcpConfigurationProperties.isTcpNodelay())
                            .option(ChannelOption.SO_KEEPALIVE, nettyTcpConfigurationProperties.isSoKeepalive())
                            .option(ChannelOption.SO_REUSEADDR, nettyTcpConfigurationProperties.isSoReuseaddr())
                            .option(ChannelOption.SO_LINGER, nettyTcpConfigurationProperties.getSoLinger()));
        }
    }
}
