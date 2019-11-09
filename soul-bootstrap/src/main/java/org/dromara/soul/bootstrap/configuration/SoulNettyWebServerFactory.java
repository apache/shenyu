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
import io.netty.channel.EventLoopGroup;
import io.netty.channel.epoll.EpollEventLoopGroup;
import io.netty.channel.epoll.EpollServerSocketChannel;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.common.utils.OSinfoUtils;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.embedded.netty.NettyServerCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import reactor.netty.ReactorNetty;
import reactor.netty.http.server.HttpServer;

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
                    .availableProcessors() << 1, 8)));

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
        public HttpServer apply(HttpServer httpServer) {
            return httpServer
                    .tcpConfiguration(tcpServer -> tcpServer.bootstrap(
                            server -> {
                                //判断Linux系统
                                EventLoopGroup boosGroup;
                                EventLoopGroup workGroup;
                                if (OSinfoUtils.isLinux()) {
                                    boosGroup = new EpollEventLoopGroup(1, SoulThreadFactory.create("soul-boss-epoll", false));
                                    workGroup = new EpollEventLoopGroup(DEFAULT_IO_WORKER_COUNT, SoulThreadFactory.create("soul-work-epoll", false));
                                    server.channel(EpollServerSocketChannel.class);
                                } else {
                                    boosGroup = new NioEventLoopGroup(1, SoulThreadFactory.create("soul-boss-nio", false));
                                    workGroup = new NioEventLoopGroup(DEFAULT_IO_WORKER_COUNT, SoulThreadFactory.create("soul-work-nio", false));
                                    server.channel(NioServerSocketChannel.class);
                                }
                                return server.group(boosGroup, workGroup)
                                        /*
                                         * 　ChannelOption.SO_BACKLOG对应的是tcp/ip协议listen函数中的backlog参数，
                                         * 函数listen(int socketfd,int backlog)用来初始化服务端可连接队列，服务端处理客户端连接请求是顺序处理的，
                                         * 所以同一时间只能处理一个客户端连接，多个客户端来的时候，服务端将不能处理的客户端连接请求放在队列中等待处理，
                                         * backlog参数指定了队列的大小
                                         */
                                        .option(ChannelOption.SO_BACKLOG, 65535)
                                        /*
                                         * ChanneOption.SO_REUSEADDR对应于套接字选项中的SO_REUSEADDR，这个参数表示允许重复使用本地地址和端口，比如，
                                         * 某个服务器进程占用了TCP的80端口进行监听，此时再次监听该端口就会返回错误，使用该参数就可以解决问题，该参数允许共用该端口，这个在服务器程序中比较常使用，
                                         * 比如某个进程非正常退出，该程序占用的端口可能要被占用一段时间才能允许其他进程使用，而且程序死掉以后，
                                         * 内核一需要一定的时间才能够释放此端口，不设置SO_REUSEADDR就无法正常使用该端口
                                         */
                                        .option(ChannelOption.SO_REUSEADDR, true)
                                        /*
                                         * Channeloption.SO_KEEPALIVE参数对应于套接字选项中的SO_KEEPALIVE，该参数用于设置TCP连接，当设置该选项以后，连接会测试链接的状态，
                                         * 这个选项用于可能长时间没有数据交流的连接。当设置该选项以后，如果在两小时内没有数据的通信时，TCP会自动发送一个活动探测数据报文。
                                         */
                                        //.option(ChannelOption.SO_KEEPALIVE, true)
                                        .option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                                        /*
                                         * 　ChannelOption.TCP_NODELAY参数对应于套接字选项中的TCP_NODELAY,该参数的使用与Nagle算法有关Nagle算法是将小的数据包组装为更大的帧然后进行发送，
                                         * 而不是输入一次发送一次,因此在数据包不足的时候会等待其他数据的到了，组装成大的数据包进行发送，虽然该方式有效提高网络的有效负载，但是却造成了延时，
                                         * 而该参数的作用就是禁止使用Nagle算法，使用于小数据即时传输，于TCP_NODELAY相对应的是TCP_CORK，
                                         * 该选项是需要等到发送的数据量最大的时候，一次性发送数据，适用于文件传输。
                                         */
                                        .childOption(ChannelOption.TCP_NODELAY, true)
                                        .childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT);

                            }
                    ));
        }
    }
}
