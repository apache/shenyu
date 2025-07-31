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

package org.apache.shenyu.plugin.mcp.server.request;

import org.apache.shenyu.plugin.base.support.BodyInserterContext;
import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.apache.shenyu.plugin.base.utils.ResponseUtils;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ReactiveHttpOutputMessage;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.ServerWebExchangeDecorator;
import reactor.core.publisher.Flux;

import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * Body Writer Exchange for MCP tool requests.
 * 
 * <p>Decorates ServerWebExchange to properly handle request body writing for different content types.
 * Supports JSON, form-urlencoded, and multipart form data formats.</p>
 *
 * @since 2.7.0.2
 */
public class BodyWriterExchange extends ServerWebExchangeDecorator {

    private final String body;

    private final String contentType;

    private final CachedBodyOutputMessage cachedBodyOutputMessage;

    public BodyWriterExchange(final ServerWebExchange delegate, final String body) {
        super(delegate);
        this.body = body;
        HttpHeaders headers = delegate.getRequest().getHeaders();
        this.contentType = headers.getFirst(HttpHeaders.CONTENT_TYPE);

        this.cachedBodyOutputMessage = ResponseUtils.newCachedBodyOutputMessage(delegate);

        initializeBody();
    }

    private void initializeBody() {
        String bodyContent = getFormattedBody();
        BodyInserter<String, ReactiveHttpOutputMessage> bodyInserter = BodyInserters.fromValue(bodyContent);

        bodyInserter.insert(cachedBodyOutputMessage, new BodyInserterContext()).subscribe();
    }

    private String getFormattedBody() {
        if (Objects.isNull(contentType)) {
            return body;
        }
        if (contentType.contains("application/json")) {
            return body;
        } else if (contentType.contains("application/x-www-form-urlencoded")) {
            return body;
        } else if (contentType.contains("multipart/form-data")) {
            return body;
        } else {
            return body;
        }
    }

    /**
     * Returns a decorated ServerHttpRequest with the correct body and headers for downstream processing.
     *
     * @return the decorated ServerHttpRequest
     */
    @Override
    public ServerHttpRequest getRequest() {
        return new ServerHttpRequestDecorator(super.getRequest()) {
            @Override
            public Flux<DataBuffer> getBody() {
                return cachedBodyOutputMessage.getBody();
            }

            @Override
            public HttpHeaders getHeaders() {
                HttpHeaders originalHeaders = super.getHeaders();
                HttpHeaders newHeaders = new HttpHeaders();
                newHeaders.putAll(originalHeaders);

                if (Objects.nonNull(body)) {
                    byte[] bodyBytes = getFormattedBody().getBytes(StandardCharsets.UTF_8);
                    newHeaders.setContentLength(bodyBytes.length);
                }

                return newHeaders;
            }
        };
    }
}