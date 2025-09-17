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

import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

public class ShenyuMcpResponseDecorator extends ServerHttpResponseDecorator {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuMcpResponseDecorator.class);

    private final StringBuilder body = new StringBuilder();

    private final CompletableFuture<String> future;

    private final String sessionId;

    private boolean isFirstChunk = true;

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
        return super.writeWith(Flux.from(body).doOnNext(buffer -> {
            byte[] bytes = new byte[buffer.readableByteCount()];
            buffer.read(bytes);
            String chunk = new String(bytes, StandardCharsets.UTF_8);
            if (isFirstChunk) {
                LOG.debug("First response chunk received for session: {}", sessionId);
                isFirstChunk = false;
            }
            LOG.debug("Received response chunk for session {}, length: {}", sessionId, chunk.length());
            synchronized (this.body) {
                this.body.append(chunk);
            }
            // Complete future early for efficiency, but safely check if already done
            if (!future.isDone()) {
                synchronized (future) {
                    if (!future.isDone()) {
                        future.complete(applyResponseTemplate(this.body.toString()));
                    }
                }
            }
        }));
    }

    @Override
    public Mono<Void> writeAndFlushWith(final Publisher<? extends Publisher<? extends DataBuffer>> body) {
        LOG.debug("Writing and flushing response data for session: {}", sessionId);
        return super.writeAndFlushWith(body);
    }

    @Override
    public Mono<Void> setComplete() {
        LOG.debug("Response completed for session: {}", sessionId);
        String responseBody;
        synchronized (this.body) {
            responseBody = this.body.toString();
        }
        LOG.debug("Final response body length for session {}: {}", sessionId, responseBody.length());
        if (!future.isDone()) {
            synchronized (future) {
                if (!future.isDone()) {
                    future.complete(applyResponseTemplate(responseBody));
                }
            }
        }
        return super.setComplete();
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