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

package org.apache.shenyu.plugin.mcp.server;

import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;

public class ShenyuMcpResponseDecorator extends ServerHttpResponseDecorator {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuMcpResponseDecorator.class);
    
    private final StringBuilder body = new StringBuilder();
    
    private final CompletableFuture<String> future;
    
    private final String sessionId;
    
    private boolean isFirstChunk = true;
    
    public ShenyuMcpResponseDecorator(final String sessionId, final CompletableFuture<String> future) {
        super(null);
        this.sessionId = sessionId;
        this.future = future;
    }
    
    @Override
    public Mono<Void> writeWith(Publisher<? extends DataBuffer> body) {
        LOG.debug("Writing response data for session: {}", sessionId);
        return super.writeWith(Flux.from(body).doOnNext(buffer -> {
            byte[] bytes = new byte[buffer.readableByteCount()];
            buffer.read(bytes);
            String chunk = new String(bytes, StandardCharsets.UTF_8);
            if (isFirstChunk) {
                LOG.debug("First response chunk received for session: {}", sessionId);
                isFirstChunk = false;
            }
            LOG.debug("Received response chunk: {}", chunk);
            this.body.append(chunk);
            if (!future.isDone()) {
                future.complete(this.body.toString());
            }
        }));
    }
    
    @Override
    public Mono<Void> writeAndFlushWith(Publisher<? extends Publisher<? extends DataBuffer>> body) {
        LOG.debug("Writing and flushing response data for session: {}", sessionId);
        return super.writeAndFlushWith(body);
    }
    
    @Override
    public Mono<Void> setComplete() {
        LOG.debug("Response completed for session: {}", sessionId);
        String responseBody = this.body.toString();
        LOG.debug("Final response body length: {}", responseBody.length());
        if (!future.isDone()) {
            future.complete(responseBody);
        }
        return super.setComplete();
    }
}