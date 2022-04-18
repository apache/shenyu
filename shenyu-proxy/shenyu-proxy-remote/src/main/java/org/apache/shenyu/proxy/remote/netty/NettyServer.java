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

package org.apache.shenyu.proxy.remote.netty;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.Channel;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.epoll.Epoll;
import io.netty.channel.epoll.EpollEventLoopGroup;
import io.netty.channel.epoll.EpollServerSocketChannel;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpRequestDecoder;
import io.netty.handler.codec.http.HttpResponseEncoder;
import io.netty.handler.stream.ChunkedWriteHandler;
import io.netty.handler.timeout.IdleStateHandler;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.proxy.remote.AbstractServer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.InetSocketAddress;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

/**
 * Shenyu NettyServer.
 * netty network management.
 */
public class NettyServer extends AbstractServer {
    
    private final Logger logger = LoggerFactory.getLogger(NettyServer.class);
    
    /**
     * service starter.
     */
    private final ServerBootstrap server;
    
    /**
     * service processing.
     */
    private EventLoopGroup bossGroup;
    
    /**
     * task processing thread.
     */
    private EventLoopGroup workerGroup;
    
    private final int threads = Runtime.getRuntime().availableProcessors() << 1;
    
    /**
     * Instantiates a new Abstract server.
     */
    public NettyServer() {
        super(null);
        this.server = new ServerBootstrap();
    }
    
    /**
     * start a server.
     *
     * @param port the port
     */
    @Override
    protected void start0(final int port) {
        createEventLoopGroup(threads);
        initServerBootstrap();
        try {
            Channel channel = server.bind(port).sync().channel();
            logger.info("Network listening,ip:{},port:{}", ((InetSocketAddress) channel.localAddress()).getHostString(), port);
            channel.closeFuture().sync();
        } catch (InterruptedException e) {
            logger.error("Error Network listening...... " + e.getMessage());
            throw new RuntimeException("Error Network listening " + e.getMessage());
        } finally {
            bossGroup.shutdownGracefully();
            workerGroup.shutdownGracefully();
        }
    }
    
    private void initServerBootstrap() {
        NettyServerHandler nettyServerHandler = new NettyServerHandler(this);
        server.group(bossGroup, workerGroup)
                .channel(Epoll.isAvailable() ? EpollServerSocketChannel.class : NioServerSocketChannel.class)
                .option(ChannelOption.SO_BACKLOG, 65535)
                .option(ChannelOption.SO_REUSEADDR, true)
                .option(ChannelOption.SO_KEEPALIVE, true)
                .option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                .childOption(ChannelOption.TCP_NODELAY, true)
                .childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                .childHandler(new ChannelInitializer<Channel>() {
                    @Override
                    protected void initChannel(final Channel channel) {
                        channel.pipeline().addLast(new IdleStateHandler(4000, 0, 0, TimeUnit.MILLISECONDS));
                        channel.pipeline().addLast("http-decoder", new HttpRequestDecoder());
                        channel.pipeline().addLast("http-aggregator", new HttpObjectAggregator(65535));
                        channel.pipeline().addLast("http-encoder", new HttpResponseEncoder());
                        channel.pipeline().addLast("chunkedWriter", new ChunkedWriteHandler());
                        channel.pipeline().addLast(nettyServerHandler);
                    }
                });
    }
    
    private void createEventLoopGroup(final int workerThreads) {
        ThreadFactory bossFactory = ShenyuThreadFactory.create("shenyu_proxy_server_boss_epoll", false);
        bossGroup = Epoll.isAvailable() ? new EpollEventLoopGroup(1, bossFactory) : new NioEventLoopGroup(1, bossFactory);
        ThreadFactory workerFactory = ShenyuThreadFactory.create("shenyu_proxy_server_work_epoll", false);
        workerGroup = Epoll.isAvailable() ? new EpollEventLoopGroup(workerThreads, workerFactory) : new NioEventLoopGroup(workerThreads, workerFactory);
    }
}
