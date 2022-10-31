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
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * bodyWriter is used to read Body.
 */
public class BodyWriter {

    private static final Logger LOG = LoggerFactory.getLogger(BodyWriter.class);

    private final ByteArrayOutputStream stream = new ByteArrayOutputStream();

    private final WritableByteChannel channel = Channels.newChannel(stream);

    private final AtomicBoolean isClosed = new AtomicBoolean(false);


    /**
     * write ByteBuffer.
     *
     * @param buffer byte buffer
     */
    public void write(final ByteBuffer buffer) {
        if (!isClosed.get()) {
            try {
                channel.write(buffer);
            } catch (IOException e) {
                isClosed.compareAndSet(false, true);
                LOG.error("write buffer Failed.", e);
            }
        }
    }

    /**
     * judge stream is empty.
     *
     * @return true: stream is empty
     */
    public boolean isEmpty() {
        return stream.size() == 0;
    }

    /**
     * get stream size.
     *
     * @return size of stream
     */
    public int size() {
        return stream.size();
    }

    /**
     * output stream value.
     *
     * @return string of stream
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
