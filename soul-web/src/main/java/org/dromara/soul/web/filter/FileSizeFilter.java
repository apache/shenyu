/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.web.filter;

import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.web.filter.support.BodyInserterContext;
import org.dromara.soul.web.filter.support.CachedBodyOutputMessage;
import org.dromara.soul.web.result.SoulResultEnum;
import org.dromara.soul.web.result.SoulResultUtils;
import org.dromara.soul.web.result.SoulResultWarp;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.lang.NonNull;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

/**
 * The type File size filter.
 *
 * @author xiaoyu(Myth)
 */
public class FileSizeFilter implements WebFilter {

    private static final long BYTES_PER_MB = 1024 * 1024;

    @Value("${file.size:10}")
    private int maxSize;

    private final List<HttpMessageReader<?>> messageReaders;

    public FileSizeFilter() {
        this.messageReaders = HandlerStrategies.withDefaults().messageReaders();
    }

    @Override
    @SuppressWarnings("unchecked")
    public Mono<Void> filter(@NonNull final ServerWebExchange exchange, @NonNull final WebFilterChain chain) {
        MediaType mediaType = exchange.getRequest().getHeaders().getContentType();
        if (MediaType.MULTIPART_FORM_DATA.isCompatibleWith(mediaType)) {
            ServerRequest serverRequest = ServerRequest.create(exchange,
                    messageReaders);
            return serverRequest.bodyToMono(DataBuffer.class)
                    .flatMap(size -> {
                        if (size.capacity() > BYTES_PER_MB * maxSize) {
                            ServerHttpResponse response = exchange.getResponse();
                            response.setStatusCode(HttpStatus.BAD_REQUEST);
                            Object error = SoulResultWarp.error(SoulResultEnum.PAYLOAD_TOO_LARGE.getCode(), SoulResultEnum.PAYLOAD_TOO_LARGE.getMsg(), null);
                            return SoulResultUtils.result(exchange, error);
                        }
                        BodyInserter bodyInserter = BodyInserters.fromPublisher(Mono.just(size), DataBuffer.class);
                        HttpHeaders headers = new HttpHeaders();
                        headers.putAll(exchange.getRequest().getHeaders());
                        headers.remove(HttpHeaders.CONTENT_LENGTH);
                        CachedBodyOutputMessage outputMessage = new CachedBodyOutputMessage(
                                exchange, headers);
                        return bodyInserter.insert(outputMessage, new BodyInserterContext())
                                .then(Mono.defer(() -> {
                                    ServerHttpRequest decorator = decorate(exchange, outputMessage);
                                    return chain.filter(exchange.mutate().request(decorator).build());

                                }));
                    });
        }
        return chain.filter(exchange);

    }

    private ServerHttpRequestDecorator decorate(final ServerWebExchange exchange, final CachedBodyOutputMessage outputMessage) {
        return new ServerHttpRequestDecorator(exchange.getRequest()) {
            @Override
            public Flux<DataBuffer> getBody() {
                return outputMessage.getBody();
            }
        };
    }
}
