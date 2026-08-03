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

package org.apache.shenyu.plugin.record.body;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.WritableByteChannel;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * BodyWriter.
 *
 * <p>A thread-safe writer that accumulates byte buffers into a byte array,
 * with an optional maximum size limit to prevent OOM in the gateway.</p>
 */
public class BodyWriter {

    private static final Logger LOG = LoggerFactory.getLogger(BodyWriter.class);

    private final int maxSize;

    private final AtomicBoolean isClosed = new AtomicBoolean(false);

    private boolean sizeExceeded;

    private final ByteArrayOutputStream stream = new ByteArrayOutputStream();

    private final WritableByteChannel channel = Channels.newChannel(stream);

    /**
     * Constructor with no size limit.
     */
    public BodyWriter() {
        this(Integer.MAX_VALUE);
    }

    /**
     * Constructor with max size limit.
     *
     * @param maxSize the maximum number of bytes to capture
     */
    public BodyWriter(final int maxSize) {
        this.maxSize = maxSize;
    }

    /**
     * Write a byte buffer. If the accumulated size exceeds maxSize, further writes are silently dropped.
     *
     * @param buffer the byte buffer to write
     */
    public void write(final ByteBuffer buffer) {
        if (!isClosed.get()) {
            if (sizeExceeded || stream.size() >= maxSize) {
                sizeExceeded = true;
                return;
            }
            try {
                channel.write(buffer);
                if (stream.size() > maxSize) {
                    sizeExceeded = true;
                }
            } catch (IOException e) {
                isClosed.compareAndSet(false, true);
                LOG.error("write buffer Failed.", e);
            }
        }
    }

    /**
     * Is empty.
     *
     * @return true if no data has been written
     */
    public boolean isEmpty() {
        return stream.size() == 0;
    }

    /**
     * Get current size.
     *
     * @return the number of bytes written so far
     */
    public int size() {
        return stream.size();
    }

    /**
     * Whether the max size limit has been exceeded.
     *
     * @return true if the body was truncated
     */
    public boolean isSizeExceeded() {
        return sizeExceeded;
    }

    /**
     * Output the captured data as a UTF-8 string and close the writer.
     * Calling output() multiple times is safe; subsequent calls return empty string.
     *
     * @return the captured body as a string, or empty string if no data
     */
    public String output() {
        if (isEmpty()) {
            return "";
        }
        try {
            isClosed.compareAndSet(false, true);
            return new String(stream.toByteArray(), StandardCharsets.UTF_8);
        } catch (Exception e) {
            LOG.error("Write failed: ", e);
            return "Write failed: " + e.getMessage();
        } finally {
            try {
                stream.close();
            } catch (IOException e) {
                LOG.error("Close stream error: ", e);
            }
            try {
                channel.close();
            } catch (IOException e) {
                LOG.error("Close channel error: ", e);
            }
        }
    }
}
