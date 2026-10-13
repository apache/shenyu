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

package org.apache.shenyu.plugin.mcp.server.response;

import com.google.gson.JsonObject;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

public class ShenyuMcpResponseDecorator extends ServerHttpResponseDecorator {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuMcpResponseDecorator.class);

    private static final int MAX_CAPTURE_BYTES = 1024 * 1024;

    private final ByteArrayOutputStream body = new ByteArrayOutputStream();

    private final CompletableFuture<String> future;

    private final String sessionId;

    private boolean isFirstChunk = true;

    private volatile boolean captureLimitExceeded;

    private final JsonObject responseTemplate;

    public ShenyuMcpResponseDecorator(final ServerHttpResponse response, final String sessionId,
            final CompletableFuture<String> future, final JsonObject responseTemplate) {
        super(response);
        this.sessionId = sessionId;
        this.future = future;
        this.responseTemplate = responseTemplate;
    }

    @Override
    public Mono<Void> writeWith(final Publisher<? extends DataBuffer> body) {
        LOG.debug("Writing response data for session: {}", sessionId);
        return super.writeWith(Flux.from(body).doOnNext(this::appendChunk))
                .doOnSuccess(unused -> completeFuture());
    }

    @Override
    public Mono<Void> writeAndFlushWith(final Publisher<? extends Publisher<? extends DataBuffer>> body) {
        LOG.debug("Writing and flushing response data for session: {}", sessionId);
        final Flux<Publisher<? extends DataBuffer>> capturedBody = Flux.from(body)
                .map(inner -> Flux.from(inner).doOnNext(this::appendChunk));
        return super.writeAndFlushWith(capturedBody)
                .doOnSuccess(unused -> completeFuture());
    }

    @Override
    public Mono<Void> setComplete() {
        LOG.debug("Response completed for session: {}", sessionId);
        completeFuture();
        return super.setComplete();
    }

    private void completeFuture() {
        if (captureLimitExceeded) {
            future.completeExceptionally(new IllegalStateException("MCP response exceeds the 1 MiB capture limit"));
            return;
        }
        byte[] responseBytes;
        synchronized (this.body) {
            responseBytes = this.body.toByteArray();
        }
        String responseBody = new String(responseBytes, StandardCharsets.UTF_8);
        LOG.debug("Final response body length for session {}: {}", sessionId, responseBody.length());
        if (!future.isDone()) {
            synchronized (future) {
                if (!future.isDone()) {
                    future.complete(applyResponseTemplate(responseBody));
                }
            }
        }
    }

    private void appendChunk(final DataBuffer buffer) {
        final int chunkLength = buffer.readableByteCount();
        if (isFirstChunk) {
            LOG.debug("First response chunk received for session: {}", sessionId);
            isFirstChunk = false;
        }
        LOG.debug("Received response chunk for session {}, length: {}", sessionId, chunkLength);
        synchronized (this.body) {
            if (captureLimitExceeded) {
                return;
            }
            try (DataBuffer.ByteBufferIterator iterator = buffer.readableByteBuffers()) {
                while (iterator.hasNext()) {
                    final ByteBuffer byteBuffer = iterator.next().asReadOnlyBuffer();
                    final int remainingCapacity = MAX_CAPTURE_BYTES - this.body.size();
                    if (byteBuffer.remaining() > remainingCapacity) {
                        captureLimitExceeded = true;
                        LOG.warn("Response body for session {} exceeds the 1 MiB capture limit", sessionId);
                        return;
                    }
                    final byte[] bytes = new byte[byteBuffer.remaining()];
                    byteBuffer.get(bytes);
                    this.body.write(bytes, 0, bytes.length);
                }
            }
        }
    }

    private String applyResponseTemplate(final String responseBody) {
        if (Objects.isNull(responseTemplate)) {
            return responseBody;
        }
        // For now, only support { "body": "{{.}}" } or { "body": "{{.field}}" }
        if (responseTemplate.has("body")) {
            String template = responseTemplate.get("body").getAsString();
            if ("{{.}}".equals(template)) {
                return responseBody;
            }
            // Support {{.field}} syntax
            if (template.startsWith("{{.") && template.endsWith("}}")) {
                String field = template.substring(3, template.length() - 2).trim();
                if (!field.isEmpty()) {
                    try {
                        com.google.gson.JsonElement json = com.google.gson.JsonParser.parseString(responseBody);
                        if (json.isJsonObject() && json.getAsJsonObject().has(field)) {
                            return json.getAsJsonObject().get(field).toString();
                        }
                    } catch (Exception e) {
                        LOG.warn("Failed to parse response body as JSON or extract field '{}': {}", field,
                                e.getMessage());
                    }
                }
            }
        }
        // Default fallback
        return responseBody;
    }
}
