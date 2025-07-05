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

import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.ServerWebExchangeDecorator;
import reactor.core.publisher.Flux;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

public class BodyWriterExchange extends ServerWebExchangeDecorator {

    private final String body;

    private final String contentType;

    public BodyWriterExchange(final ServerWebExchange delegate, final String body) {
        super(delegate);
        this.body = body;
        HttpHeaders headers = delegate.getRequest().getHeaders();
        this.contentType = headers.getFirst(HttpHeaders.CONTENT_TYPE);
    }

    @Override
    public ServerHttpRequest getRequest() {
        return new ServerHttpRequestDecorator(super.getRequest()) {
            @Override
            public Flux<DataBuffer> getBody() {
                DataBufferFactory bufferFactory = BodyWriterExchange.this.getResponse().bufferFactory();
                byte[] bytes = getBodyBytes();
                DataBuffer buffer = bufferFactory.wrap(bytes);
                return Flux.just(buffer);
            }
        };
    }

    private byte[] getBodyBytes() {
        if (Objects.isNull(contentType)) {
            return body.getBytes(StandardCharsets.UTF_8);
        }
        if (contentType.contains("application/json")) {
            return body.getBytes(StandardCharsets.UTF_8);
        } else if (contentType.contains("application/x-www-form-urlencoded")) {
            // body must be in key1=val1&key2=val2 format
            return body.getBytes(StandardCharsets.UTF_8);
        } else if (contentType.contains("multipart/form-data")) {
            // Simple handling here, just return the string. For complex scenarios, use
            // MultipartBodyBuilder.
            return body.getBytes(StandardCharsets.UTF_8);
        } else {
            return body.getBytes(StandardCharsets.UTF_8);
        }
    }

    // Optional: Helper method to convert Map<String, Object> to
    // x-www-form-urlencoded string
    public static String toFormUrlEncoded(final Map<String, Object> map) {
        return map.entrySet().stream()
                .map(e -> e.getKey() + "=" + urlEncode(e.getValue().toString()))
                .collect(Collectors.joining("&"));
    }

    private static String urlEncode(final String value) {
        return URLEncoder.encode(value, StandardCharsets.UTF_8);
    }
}