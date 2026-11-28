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

package org.apache.shenyu.plugin.shadow.capture;

import org.springframework.core.io.buffer.DataBuffer;

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;

/**
 * Bounded copy of a request body read by the primary request.
 *
 * <p>The capture observes buffers as the primary consumes them and never
 * retains a buffer, so the primary request keeps its own backpressure and
 * buffer ownership. Once the bound is reached the copy stops but the primary
 * stream continues untouched.</p>
 */
public final class ShadowBodyCapture {

    private static final int INITIAL_CAPACITY = 8 * 1024;

    private static final byte[] EMPTY = new byte[0];

    private final int maxBytes;

    private final ByteArrayOutputStream buffer;

    private boolean truncated;

    public ShadowBodyCapture(final int maxBytes) {
        this.maxBytes = Math.max(maxBytes, 0);
        this.buffer = new ByteArrayOutputStream(Math.min(this.maxBytes, INITIAL_CAPACITY));
    }

    /**
     * Append the readable bytes of one request buffer without changing its
     * read position.
     *
     * @param dataBuffer the buffer read by the primary request
     */
    public void append(final DataBuffer dataBuffer) {
        int readable = dataBuffer.readableByteCount();
        if (readable <= 0) {
            return;
        }
        int remaining = maxBytes - buffer.size();
        if (readable > remaining) {
            truncated = true;
            copy(dataBuffer, Math.max(remaining, 0));
            return;
        }
        copy(dataBuffer, readable);
    }

    /**
     * Whether the request body exceeded the configured bound.
     *
     * @return true when the copy stopped before the body ended
     */
    public boolean isTruncated() {
        return truncated;
    }

    /**
     * Copy of the captured bytes.
     *
     * @return the captured body, empty when nothing was read
     */
    public byte[] bytes() {
        return buffer.size() == 0 ? EMPTY : buffer.toByteArray();
    }

    private void copy(final DataBuffer dataBuffer, final int length) {
        if (length <= 0) {
            return;
        }
        ByteBuffer destination = ByteBuffer.allocate(length);
        dataBuffer.toByteBuffer(dataBuffer.readPosition(), destination, 0, length);
        buffer.write(destination.array(), 0, length);
    }
}
