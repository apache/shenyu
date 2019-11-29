/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.bootstrap.configuration;

import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.ChannelOption;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.embedded.netty.NettyServerCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import reactor.netty.ReactorNetty;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

/**
 * The type Soul netty web server factory.
 *
 * @author xiaoyu
 */
@Configuration
public class SoulNettyWebServerFactory {

    private static final int DEFAULT_IO_WORKER_COUNT = Integer.parseInt(System.getProperty(
            ReactorNetty.IO_WORKER_COUNT,
            "" + Math.max(Runtime.getRuntime()
                    .availableProcessors() << 1, 16)));

    /**
     * Netty reactive web server factory netty reactive web server factory.
     *
     * @return the netty reactive web server factory
     */
    @Bean
    public NettyReactiveWebServerFactory nettyReactiveWebServerFactory() {
        NettyReactiveWebServerFactory webServerFactory = new NettyReactiveWebServerFactory();
        webServerFactory.addServerCustomizers(new EventLoopNettyCustomizer());
        return webServerFactory;
    }

    private static class EventLoopNettyCustomizer implements NettyServerCustomizer {

        @Override
        public HttpServer apply(final HttpServer httpServer) {
            return httpServer
                    .tcpConfiguration(tcpServer -> tcpServer
                            .runOn(LoopResources.create("soul-netty", 1, DEFAULT_IO_WORKER_COUNT, true), false)
                            .selectorOption(ChannelOption.SO_REUSEADDR, true)
                            .selectorOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                            .option(ChannelOption.TCP_NODELAY, true)
                            .option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT));
        }
    }
}
