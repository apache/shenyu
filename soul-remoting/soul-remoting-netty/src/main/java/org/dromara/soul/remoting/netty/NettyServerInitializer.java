/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.remoting.netty;

import io.netty.channel.ChannelInitializer;
import io.netty.channel.socket.nio.NioSocketChannel;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpServerCodec;
import io.netty.handler.codec.http.websocketx.extensions.compression.WebSocketServerCompressionHandler;
import io.netty.handler.stream.ChunkedWriteHandler;

/**
 * The type Netty server initializer.
 *
 * @author xiaoyu
 */
public class NettyServerInitializer extends ChannelInitializer<NioSocketChannel> {

    private final NettyServerHandler serverHandler;

    /**
     * Instantiates a new Netty server initializer.
     *
     * @param serverHandler the server handler
     */
    NettyServerInitializer(NettyServerHandler serverHandler) {
        this.serverHandler = serverHandler;
    }

    @Override
    protected void initChannel(NioSocketChannel channel) {
        NettyCodec codec = new NettyCodec();
        channel.pipeline().addLast("http", new HttpServerCodec());
        channel.pipeline().addLast("websocket", new WebSocketServerCompressionHandler());
        channel.pipeline().addLast("http-aggregator", new HttpObjectAggregator(1024 * 1024 * 64));
        channel.pipeline().addLast("encode", codec.getEncoder());
        channel.pipeline().addLast("decode", codec.getDecoder());
        channel.pipeline().addLast("chunkedWriter", new ChunkedWriteHandler());
        channel.pipeline().addLast(serverHandler);
    }
}
