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

import io.netty.buffer.Unpooled;
import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.codec.http.DefaultHttpContent;
import io.netty.handler.codec.http.LastHttpContent;
import io.netty.util.ReferenceCountUtil;
import org.apache.shenyu.common.exception.StreamIdleTimeoutException;
import org.apache.shenyu.plugin.httpclient.ResponseBodyIdleTimeout;
import org.junit.jupiter.api.Test;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ResponseBodyIdleTimeoutHandlerTest {

    private final AtomicLong nanos = new AtomicLong();

    @Test
    void emptyContentDoesNotPostponeFirstBodyTimeout() {
        EmbeddedChannel channel = new EmbeddedChannel();
        channel.freezeTime();
        AtomicInteger removals = install(channel);
        try {
            advance(channel, 9000);
            channel.writeInbound(new DefaultHttpContent(Unpooled.EMPTY_BUFFER));
            ReferenceCountUtil.release(channel.readInbound());
            assertTrue(channel.isActive());
            advance(channel, 1001);
            assertThrows(StreamIdleTimeoutException.class, channel::checkException);
            assertFalse(channel.isActive());
            assertEquals(1, removals.get());
        } finally {
            channel.finishAndReleaseAll();
        }
    }

    @Test
    void bodyProgressRestartsTimerAndLastContentRemovesIt() {
        EmbeddedChannel channel = new EmbeddedChannel();
        channel.freezeTime();
        AtomicInteger removals = install(channel);
        try {
            for (int chunk = 0; chunk < 4; chunk++) {
                advance(channel, 7000);
                channel.writeInbound(new DefaultHttpContent(Unpooled.buffer(1).writeByte(chunk)));
                ReferenceCountUtil.release(channel.readInbound());
            }
            channel.writeInbound(LastHttpContent.EMPTY_LAST_CONTENT);
            ReferenceCountUtil.release(channel.readInbound());
            advance(channel, 20000);
            assertTrue(channel.isActive());
            assertNull(channel.pipeline().get(ResponseBodyIdleTimeoutHandler.class));
            assertEquals(1, removals.get());
        } finally {
            channel.finishAndReleaseAll();
        }
    }

    @Test
    void expiresDuringSilenceAfterBodyProgress() {
        EmbeddedChannel channel = new EmbeddedChannel();
        channel.freezeTime();
        install(channel);
        try {
            advance(channel, 9000);
            channel.writeInbound(new DefaultHttpContent(Unpooled.wrappedBuffer(new byte[]{1})));
            ReferenceCountUtil.release(channel.readInbound());
            advance(channel, 9000);
            assertTrue(channel.isActive());
            advance(channel, 1001);
            assertThrows(StreamIdleTimeoutException.class, channel::checkException);
            assertFalse(channel.isActive());
        } finally {
            channel.finishAndReleaseAll();
        }
    }

    private AtomicInteger install(final EmbeddedChannel channel) {
        AtomicInteger removals = new AtomicInteger();
        channel.pipeline().addLast(new ResponseBodyIdleTimeoutHandler(new ResponseBodyIdleTimeout(10000L), removals::incrementAndGet, nanos::get));
        return removals;
    }

    private void advance(final EmbeddedChannel channel, final long millis) {
        nanos.addAndGet(TimeUnit.MILLISECONDS.toNanos(millis));
        channel.advanceTimeBy(millis, TimeUnit.MILLISECONDS);
        channel.runScheduledPendingTasks();
        channel.runPendingTasks();
    }
}
