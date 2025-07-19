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

package org.apache.shenyu.web.filter;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.support.BodyInserterContext;
import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.apache.shenyu.plugin.base.utils.ResponseUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ReactiveHttpOutputMessage;
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
import java.util.function.Function;

/**
 * The type File size filter.
 */
public class FileSizeFilter implements WebFilter {
    private static final Logger LOG = LoggerFactory.getLogger(FileSizeFilter.class);

    private final int fileMaxSize;

    private final List<HttpMessageReader<?>> messageReaders;

    public FileSizeFilter(final int fileMaxSize) {
        HandlerStrategies handlerStrategies = HandlerStrategies.builder().codecs(configurer -> configurer.defaultCodecs().maxInMemorySize(-1)).build();
        this.messageReaders = handlerStrategies.messageReaders();
        this.fileMaxSize = fileMaxSize;
    }

    @Override
    @NonNull
    public Mono<Void> filter(@NonNull final ServerWebExchange exchange, @NonNull final WebFilterChain chain) {
        MediaType mediaType = exchange.getRequest().getHeaders().getContentType();
        if (MediaType.MULTIPART_FORM_DATA.isCompatibleWith(mediaType)) {
            ServerRequest serverRequest = ServerRequest.create(exchange,
                    messageReaders);
            return serverRequest.bodyToMono(DataBuffer.class)
                    .flatMap(dataBuffer -> {
                        if (dataBuffer.capacity() > Constants.BYTES_PER_MB * fileMaxSize) {
                            ServerHttpResponse response = exchange.getResponse();
                            response.setStatusCode(HttpStatus.BAD_REQUEST);
                            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.PAYLOAD_TOO_LARGE);
                            LOG.info("The file size exceeds the limit. The actual size is {}M , response:{}", dataBuffer.capacity() / Constants.BYTES_PER_MB, error);
                            return WebFluxResultUtils.result(exchange, error);
                        }
                        BodyInserter<Mono<DataBuffer>, ReactiveHttpOutputMessage> bodyInsert = BodyInserters.fromPublisher(Mono.just(dataBuffer), DataBuffer.class);
                        HttpHeaders headers = new HttpHeaders();
                        headers.putAll(exchange.getRequest().getHeaders());
                        headers.remove(HttpHeaders.CONTENT_LENGTH);
                        CachedBodyOutputMessage outputMessage = new CachedBodyOutputMessage(
                                exchange, headers);
                        return bodyInsert.insert(outputMessage, new BodyInserterContext())
                                .then(Mono.defer(() -> {
                                    ServerHttpRequest decorator = decorate(exchange, outputMessage);
                                    return chain.filter(exchange.mutate().request(decorator).build());
                                })).doFinally(signalType -> DataBufferUtils.release(dataBuffer))
                                .onErrorResume((Function<Throwable, Mono<Void>>) throwable -> ResponseUtils.release(outputMessage, throwable));
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
