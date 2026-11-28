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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import reactor.core.publisher.Flux;

import java.nio.ByteBuffer;
import java.util.Arrays;

/**
 * Copies the first upload without changing the read position or retaining pooled buffers.
 * Only a completed upload can be sent again.
 */
final class AiUploadReplayBuffer {

    private byte[] bytes = new byte[0];

    private int size;

    private volatile boolean complete;

    Flux<DataBuffer> capture(final Flux<DataBuffer> upload) {
        return upload.doOnNext(this::append).doOnComplete(this::finish);
    }

    private synchronized void append(final DataBuffer chunk) {
        int length = chunk.readableByteCount();
        if (size < 0) {
            return;
        }
        if (length > AiProxyBodies.MAX_BODY_BYTES - size) {
            discard();
            return;
        }
        int required = size + length;
        if (required > bytes.length) {
            bytes = Arrays.copyOf(bytes, Math.min(AiProxyBodies.MAX_BODY_BYTES, Math.max(required, 2 * bytes.length)));
        }
        chunk.toByteBuffer(chunk.readPosition(), ByteBuffer.wrap(bytes), size, length);
        size = required;
    }

    private synchronized void finish() {
        complete = size >= 0;
    }

    synchronized void discard() {
        size = -1;
        bytes = new byte[0];
        complete = false;
    }

    boolean isComplete() {
        return complete;
    }

    synchronized Flux<DataBuffer> replay() {
        return size == 0 ? Flux.empty() : Flux.just(DefaultDataBufferFactory.sharedInstance.wrap(ByteBuffer.wrap(bytes, 0, size).asReadOnlyBuffer()));
    }
}
