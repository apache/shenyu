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

package org.apache.shenyu.plugin.logging.common.body;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.WritableByteChannel;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * bodyWriter is used to read Body.
 *
 * <p>The body is buffered only up to {@code maxBodySize} bytes. Once the limit is exceeded the
 * buffer is released (so it can be garbage collected before the request completes) and only the
 * total size keeps being tracked. All methods are
 * synchronized because reactive signals (e.g. cancel-triggered {@code doFinally}) are not
 * guaranteed to be serialized with {@code onNext}.</p>
 */
public class BodyWriter {

    private static final Logger LOG = LoggerFactory.getLogger(BodyWriter.class);

    private final int maxBodySize;

    /**
     * Lazily allocated on the first buffered write and released on overflow; guarded by {@code this}.
     */
    private ByteArrayOutputStream stream;

    private WritableByteChannel channel;

    private long size;

    private boolean overflow;

    private boolean closed;

    /**
     * Create a writer without a body limit (legacy behavior).
     */
    public BodyWriter() {
        this(Integer.MAX_VALUE);
    }

    /**
     * Create a writer that stops buffering once the body exceeds the limit.
     *
     * @param maxBodySize maximum buffered body size in bytes; non-positive values buffer nothing
     */
    public BodyWriter(final int maxBodySize) {
        this.maxBodySize = Math.max(0, maxBodySize);
    }

    /**
     * write ByteBuffer.
     *
     * @param buffer byte buffer
     */
    public synchronized void write(final ByteBuffer buffer) {
        if (closed || !buffer.hasRemaining()) {
            return;
        }
        size += buffer.remaining();
        if (overflow) {
            return;
        }
        if (size > maxBodySize) {
            // release the buffered prefix so its array can be collected before the request ends;
            // only the total size is tracked from now on
            overflow = true;
            stream = null;
            channel = null;
            return;
        }
        if (Objects.isNull(stream)) {
            stream = new ByteArrayOutputStream();
            channel = Channels.newChannel(stream);
        }
        try {
            channel.write(buffer);
        } catch (IOException e) {
            closed = true;
            LOG.error("write buffer Failed.", e);
        }
    }

    /**
     * Judge whether no body bytes have been observed.
     *
     * @return true when the body is empty
     */
    public synchronized boolean isEmpty() {
        return size == 0;
    }

    /**
     * Get the observed body size, including bytes discarded after exceeding the limit.
     *
     * @return observed byte count, capped at {@link Integer#MAX_VALUE}
     */
    public synchronized int size() {
        return (int) Math.min(size, Integer.MAX_VALUE);
    }

    /**
     * Judge whether the body exceeded the limit and was discarded.
     *
     * @return true when the body has been discarded
     */
    public synchronized boolean isLimitExceeded() {
        return overflow;
    }

    /**
     * Output the buffered body and stop accepting further writes.
     *
     * @return string of stream, or an empty string if the body is empty or exceeds the limit
     */
    public synchronized String output() {
        closed = true;
        if (size == 0 || overflow || Objects.isNull(stream)) {
            return "";
        }
        return new String(stream.toByteArray(), StandardCharsets.UTF_8);
    }
}
