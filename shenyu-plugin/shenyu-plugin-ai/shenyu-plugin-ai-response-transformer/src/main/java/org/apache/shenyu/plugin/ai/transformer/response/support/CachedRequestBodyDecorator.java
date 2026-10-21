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

package org.apache.shenyu.plugin.ai.transformer.response.support;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.lang.NonNull;
import reactor.core.publisher.Flux;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * Captures a bounded copy of the request body while the downstream chain reads it.
 *
 * <p>The forwarded body is left untouched: buffers are copied through a read-only view without
 * moving their read position, so the plugin or client that forwards the request still receives
 * every byte. The captured copy lets the response phase build a prompt from a request body that
 * the downstream chain has already consumed.
 *
 * <p>A body that exceeds the snapshot limit is not captured; it is still forwarded unchanged and
 * the prompt simply keeps the previous empty request body, so memory stays bounded.
 */
public final class CachedRequestBodyDecorator extends ServerHttpRequestDecorator {

    /**
     * The default maximum number of request body bytes kept for the ai prompt.
     */
    public static final int DEFAULT_MAX_BYTES = 1024 * 1024;

    private static final Logger LOG = LoggerFactory.getLogger(CachedRequestBodyDecorator.class);

    private final int maxBytes;

    private final ByteArrayOutputStream snapshot = new ByteArrayOutputStream();

    private boolean overflow;

    /**
     * Creates a decorator that keeps up to {@link #DEFAULT_MAX_BYTES} body bytes.
     *
     * @param request the request to decorate
     */
    public CachedRequestBodyDecorator(final ServerHttpRequest request) {
        this(request, DEFAULT_MAX_BYTES);
    }

    /**
     * Creates a decorator with an explicit snapshot limit.
     *
     * @param request the request to decorate
     * @param maxBytes the maximum number of body bytes kept for the snapshot
     */
    public CachedRequestBodyDecorator(final ServerHttpRequest request, final int maxBytes) {
        super(request);
        this.maxBytes = maxBytes;
    }

    /**
     * Returns the original body stream and records a copy of every forwarded buffer.
     *
     * @return the forwarded body stream
     */
    @Override
    @NonNull
    public Flux<DataBuffer> getBody() {
        return super.getBody().doOnNext(this::record);
    }

    private synchronized void record(final DataBuffer dataBuffer) {
        if (overflow || Objects.isNull(dataBuffer)) {
            return;
        }
        int readable = dataBuffer.readableByteCount();
        if (readable <= 0) {
            return;
        }
        if (snapshot.size() + readable > maxBytes) {
            overflow = true;
            snapshot.reset();
            LOG.warn("Request body exceeds the {} byte snapshot limit and is not sent to the ai model", maxBytes);
            return;
        }
        byte[] bytes = new byte[readable];
        dataBuffer.toByteBuffer(dataBuffer.readPosition(), readable).get(bytes);
        snapshot.write(bytes, 0, readable);
    }

    /**
     * Returns the captured request body decoded as UTF-8.
     *
     * <p>Returns an empty string when the downstream chain never read the body or the body
     * exceeded the snapshot limit, which keeps the previous prompt content for those requests.
     *
     * @return the captured request body
     */
    public synchronized String bodyAsString() {
        return snapshot.toString(StandardCharsets.UTF_8);
    }
}
