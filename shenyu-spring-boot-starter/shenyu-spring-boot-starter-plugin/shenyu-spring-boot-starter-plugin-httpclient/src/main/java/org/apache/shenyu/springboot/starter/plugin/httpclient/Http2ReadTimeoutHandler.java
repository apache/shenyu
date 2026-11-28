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

import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.channel.ChannelPipeline;
import io.netty.handler.codec.http2.Http2StreamChannel;
import io.netty.handler.timeout.ReadTimeoutException;
import io.netty.handler.timeout.ReadTimeoutHandler;
import io.netty.util.Attribute;
import io.netty.util.AttributeKey;
import reactor.netty.Connection;
import reactor.netty.NettyPipeline;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * Applies the physical connection's read timeout to streams without an active rule body timer.
 * Netty registers all multiplexed children on their parent's event loop.
 */
final class Http2ReadTimeoutHandler extends ReadTimeoutHandler {

    private static final AttributeKey<Set<Channel>> STREAMS = AttributeKey.valueOf(Http2ReadTimeoutHandler.class, "streams");

    private static final String READ_PROGRESS = "shenyuHttp2ReadProgress";

    private int protectedBodies;

    Http2ReadTimeoutHandler(final long millis) {
        super(millis, TimeUnit.MILLISECONDS);
    }

    static void registerStream(final Connection connection) {
        Channel stream = connection.channel();
        if (stream instanceof Http2StreamChannel) {
            // The first stream can start during SETTINGS processing, before doOnConnected
            // installs the parent's read handler. Register against the physical channel itself.
            Set<Channel> streams = openStreams(stream.parent());
            streams.add(stream);
            connection.onTerminate().subscribe(null, null, () -> streams.remove(stream));
        }
    }

    static Runnable protectStream(final Channel parent) {
        ChannelPipeline pipeline = parent.pipeline();
        Http2ReadTimeoutHandler handler = pipeline.get(Http2ReadTimeoutHandler.class);
        if (Objects.isNull(handler)) {
            return () -> { };
        }
        if (handler.protectedBodies++ == 0) {
            // The normal read handler is downstream of the multiplexer, which consumes child
            // frames. While preserving a body, count physical reads before frame dispatch.
            pipeline.addBefore(NettyPipeline.HttpCodec, READ_PROGRESS, new ChannelInboundHandlerAdapter() {
                @Override
                public void channelRead(final ChannelHandlerContext ctx, final Object message) {
                    handler.resetReadTimeout();
                    ctx.fireChannelRead(message);
                }
            });
        }
        handler.resetReadTimeout();
        return () -> {
            if (--handler.protectedBodies == 0 && Objects.nonNull(pipeline.get(READ_PROGRESS))) {
                pipeline.remove(READ_PROGRESS);
            }
        };
    }

    private static Set<Channel> openStreams(final Channel parent) {
        Attribute<Set<Channel>> streams = parent.attr(STREAMS);
        if (Objects.isNull(streams.get())) {
            streams.set(new HashSet<>());
        }
        return streams.get();
    }

    @Override
    protected void readTimedOut(final ChannelHandlerContext ctx) throws Exception {
        if (protectedBodies == 0) {
            super.readTimedOut(ctx);
            return;
        }
        // Closing a child may synchronously terminate it and remove it from the registry.
        List<Channel> unprotectedStreams = openStreams(ctx.channel()).stream()
                .filter(stream -> Objects.isNull(stream.pipeline().get(ResponseBodyIdleTimeoutHandler.class)))
                .toList();
        for (Channel stream : unprotectedStreams) {
            stream.pipeline().fireExceptionCaught(ReadTimeoutException.INSTANCE);
            stream.close();
        }
    }
}
