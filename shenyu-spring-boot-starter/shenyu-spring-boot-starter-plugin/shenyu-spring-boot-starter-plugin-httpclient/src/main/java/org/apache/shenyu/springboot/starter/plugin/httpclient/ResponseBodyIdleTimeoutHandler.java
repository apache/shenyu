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

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.channel.ChannelPipeline;
import io.netty.handler.codec.http.HttpContent;
import io.netty.handler.codec.http.LastHttpContent;
import io.netty.handler.codec.http2.Http2StreamChannel;
import io.netty.handler.timeout.ReadTimeoutHandler;
import io.netty.util.concurrent.ScheduledFuture;
import org.apache.shenyu.common.exception.StreamIdleTimeoutException;
import org.apache.shenyu.plugin.httpclient.ResponseBodyIdleTimeout;
import reactor.netty.Connection;
import reactor.netty.NettyPipeline;
import reactor.netty.http.client.HttpClientResponse;

import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.function.LongSupplier;

/**
 * Measures gaps in decoded upstream body data, from final headers through the last content.
 */
final class ResponseBodyIdleTimeoutHandler extends ChannelInboundHandlerAdapter {

    private static final String READ_TIMEOUT = "shenyuReadTimeout";

    private final ResponseBodyIdleTimeout timeout;

    private final Runnable restoreReadTimeout;

    private final LongSupplier nanoTime;

    private final long idleNanos;

    private long deadline;

    private ScheduledFuture<?> timer;

    ResponseBodyIdleTimeoutHandler(final ResponseBodyIdleTimeout timeout, final Runnable restoreReadTimeout, final LongSupplier nanoTime) {
        this.timeout = timeout;
        this.restoreReadTimeout = restoreReadTimeout;
        this.nanoTime = nanoTime;
        this.idleNanos = TimeUnit.MILLISECONDS.toNanos(timeout.millis());
    }

    static void addReadTimeout(final Connection connection, final long millis) {
        if (Objects.nonNull(connection.channel().pipeline().get(NettyPipeline.H2MultiplexHandler))) {
            connection.addHandlerLast(READ_TIMEOUT, new Http2ReadTimeoutHandler(millis));
        } else {
            connection.addHandlerLast(READ_TIMEOUT, new ReadTimeoutHandler(millis, TimeUnit.MILLISECONDS));
        }
    }

    static void onResponse(final HttpClientResponse response, final Connection connection) {
        ResponseBodyIdleTimeout timeout = response.currentContextView().getOrDefault(ResponseBodyIdleTimeout.class, null);
        if (Objects.isNull(timeout) || response.status().code() < 200) {
            return;
        }
        ChannelPipeline pipeline = connection.channel().pipeline();
        Runnable restore = switchReadTimeout(connection);
        // An early response can precede onOutboundComplete. Occupying Reactor's reserved name
        // prevents it from installing a second header timer after the body timer has started.
        String name = NettyPipeline.ResponseTimeoutHandler;
        if (Objects.nonNull(pipeline.get(name))) {
            pipeline.remove(name);
            name = ResponseBodyIdleTimeoutHandler.class.getSimpleName();
        }
        // Connection also removes this handler at operation termination. A FullHttpResponse
        // already inside Reactor's bridge will never traverse the newly inserted handler.
        connection.addHandlerLast(name, new ResponseBodyIdleTimeoutHandler(timeout, restore, System::nanoTime));
    }

    private static Runnable switchReadTimeout(final Connection connection) {
        if (connection.channel() instanceof Http2StreamChannel) {
            return Http2ReadTimeoutHandler.protectStream(connection.channel().parent());
        }
        ChannelPipeline pipeline = connection.channel().pipeline();
        ReadTimeoutHandler read = (ReadTimeoutHandler) pipeline.get(READ_TIMEOUT);
        if (Objects.isNull(read)) {
            return () -> { };
        }
        pipeline.remove(read);
        return () -> {
            if (connection.channel().isActive()) {
                addReadTimeout(connection, read.getReaderIdleTimeInMillis());
            }
        };
    }

    @Override
    public void handlerAdded(final ChannelHandlerContext ctx) {
        deadline = nanoTime.getAsLong() + idleNanos;
        scheduleCheck(ctx, idleNanos);
    }

    @Override
    public void channelRead(final ChannelHandlerContext ctx, final Object message) {
        if (message instanceof LastHttpContent) {
            ctx.pipeline().remove(this);
        } else if (message instanceof HttpContent content && content.content().isReadable()) {
            deadline = nanoTime.getAsLong() + idleNanos;
        }
        ctx.fireChannelRead(message);
    }

    @Override
    public void handlerRemoved(final ChannelHandlerContext ctx) {
        timer.cancel(false);
        restoreReadTimeout.run();
    }

    private void scheduleCheck(final ChannelHandlerContext ctx, final long delayNanos) {
        timer = ctx.executor().schedule(() -> {
            long remaining = deadline - nanoTime.getAsLong();
            if (remaining > 0) {
                scheduleCheck(ctx, remaining);
            } else {
                StreamIdleTimeoutException failure = timeout.expire();
                // Signalling a waiting writer can synchronously cancel and close this operation.
                if (ctx.channel().isActive()) {
                    ctx.fireExceptionCaught(failure);
                    ctx.close();
                }
            }
        }, delayNanos, TimeUnit.NANOSECONDS);
    }
}
