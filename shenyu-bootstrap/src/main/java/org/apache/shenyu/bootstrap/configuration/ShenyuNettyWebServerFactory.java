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

package org.apache.shenyu.bootstrap.configuration;

import io.netty.channel.ChannelOption;
import io.netty.channel.WriteBufferWaterMark;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.embedded.netty.NettyServerCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

/**
 * The type shenyu netty web server factory.
 */
@Configuration
public class ShenyuNettyWebServerFactory {

    /**
     * Netty reactive web server factory netty reactive web server factory.
     *
     * @return the netty reactive web server factory
     */
    @Bean
    public NettyReactiveWebServerFactory nettyReactiveWebServerFactory() {
        NettyReactiveWebServerFactory webServerFactory = new NettyReactiveWebServerFactory();
        NettyTcpConfig nettyTcpConfig = nettyTcpConfig();
        webServerFactory.addServerCustomizers(new EventLoopNettyCustomizer(nettyTcpConfig));
        return webServerFactory;
    }

    /**
     * Netty tcp config.
     *
     * @return the netty tcp config
     */
    @Bean
    public NettyTcpConfig nettyTcpConfig() {
        return new NettyTcpConfig();
    }

    private static class EventLoopNettyCustomizer implements NettyServerCustomizer {

        private final NettyTcpConfig nettyTcpConfig;

        EventLoopNettyCustomizer(final NettyTcpConfig nettyTcpConfig) {
            this.nettyTcpConfig = nettyTcpConfig;
        }

        @Override
        public HttpServer apply(final HttpServer httpServer) {
            return httpServer
                    .tcpConfiguration(tcpServer -> tcpServer
                            .runOn(LoopResources.create("shenyu-netty", nettyTcpConfig.getSelectCount(), nettyTcpConfig.getWorkerCount(), true))
                            .selectorOption(ChannelOption.SO_BACKLOG, nettyTcpConfig.getSoBacklog())
                            .selectorOption(ChannelOption.SO_REUSEADDR, nettyTcpConfig.isSoReuseaddr())
                            .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, nettyTcpConfig.getConnectTimeoutMillis())
                            .option(ChannelOption.WRITE_BUFFER_WATER_MARK, new WriteBufferWaterMark(nettyTcpConfig.getWriteBufferLowWaterMark(),
                                    nettyTcpConfig.getWriteBufferHighWaterMark()))
                            .option(ChannelOption.SO_KEEPALIVE, nettyTcpConfig.isSoKeepalive())
                            .option(ChannelOption.SO_REUSEADDR, nettyTcpConfig.isSoReuseaddr())
                            .option(ChannelOption.SO_LINGER, nettyTcpConfig.getSoLinger())
                            .option(ChannelOption.TCP_NODELAY, nettyTcpConfig.isTcpNodelay()));
        }
    }
}
