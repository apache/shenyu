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
import org.springframework.core.io.buffer.DataBufferLimitException;
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

/**
 * The type File size filter.
 */
public class FileSizeFilter implements WebFilter {
    private static final Logger LOG = LoggerFactory.getLogger(FileSizeFilter.class);

    private final int fileMaxSize;

    /**
     * The max number of bytes buffered while reading a multipart body, or -1 when the configured
     * max size is not positive (in that case every multipart request is rejected without buffering).
     */
    private final int maxInMemorySize;

    private final List<HttpMessageReader<?>> messageReaders;

    public FileSizeFilter(final int fileMaxSize) {
        this.fileMaxSize = fileMaxSize;
        this.maxInMemorySize = maxInMemorySize(fileMaxSize);
        HandlerStrategies handlerStrategies = HandlerStrategies.builder()
                .codecs(configurer -> configurer.defaultCodecs().maxInMemorySize(this.maxInMemorySize)).build();
        this.messageReaders = handlerStrategies.messageReaders();
    }

    @Override
    @NonNull
    public Mono<Void> filter(@NonNull final ServerWebExchange exchange, @NonNull final WebFilterChain chain) {
        MediaType mediaType = exchange.getRequest().getHeaders().getContentType();
        if (MediaType.MULTIPART_FORM_DATA.isCompatibleWith(mediaType)) {
            // a non-positive max size rejects every multipart request, so its body is not buffered at all
            if (fileMaxSize <= 0) {
                return payloadTooLarge(exchange, "The configured max size is " + fileMaxSize + "M");
            }
            ServerRequest serverRequest = ServerRequest.create(exchange,
                    messageReaders);
            return serverRequest.bodyToMono(DataBuffer.class)
                    .flatMap(dataBuffer -> {
                        if (dataBuffer.capacity() > maxInMemorySize) {
                            final int actualSize = dataBuffer.capacity();
                            DataBufferUtils.release(dataBuffer);
                            return payloadTooLarge(exchange,
                                    "The actual size is " + actualSize / Constants.BYTES_PER_MB + "M");
                        }
                        BodyInserter<Mono<DataBuffer>, ReactiveHttpOutputMessage> bodyInsert = BodyInserters
                                .fromPublisher(Mono.just(dataBuffer), DataBuffer.class);
                        HttpHeaders headers = new HttpHeaders();
                        headers.putAll(exchange.getRequest().getHeaders());
                        headers.remove(HttpHeaders.CONTENT_LENGTH);
                        CachedBodyOutputMessage outputMessage = new CachedBodyOutputMessage(
                                exchange, headers);
                        return bodyInsert.insert(outputMessage, new BodyInserterContext())
                                .then(Mono.<Void>defer(() -> {
                                    ServerHttpRequest decorator = decorate(exchange, outputMessage);
                                    return chain.filter(exchange.mutate().request(decorator).build());
                                }))
                                .doFinally(signalType -> DataBufferUtils.release(dataBuffer))
                                .onErrorResume(throwable -> ResponseUtils.release(outputMessage, throwable));
                    })
                    .onErrorResume(DataBufferLimitException.class,
                            e -> payloadTooLarge(exchange, "The max size is " + fileMaxSize + "M"));
        }
        return chain.filter(exchange);

    }

    /**
     * Reject the request as payload too large.
     *
     * @param exchange the exchange
     * @param sizeDetail the size detail used for logging
     * @return the result
     */
    private Mono<Void> payloadTooLarge(final ServerWebExchange exchange, final String sizeDetail) {
        ServerHttpResponse response = exchange.getResponse();
        response.setStatusCode(HttpStatus.BAD_REQUEST);
        Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.PAYLOAD_TOO_LARGE);
        LOG.info("The file size exceeds the limit. {} , response:{}", sizeDetail, error);
        return WebFluxResultUtils.result(exchange, error);
    }

    /**
     * The max number of bytes to buffer while reading a multipart body.
     * A bounded value makes the codec fail as soon as the body exceeds the limit,
     * instead of buffering an unbounded body in memory.
     *
     * @param fileMaxSize the file max size in MB
     * @return the max number of bytes, or -1 for a non-positive file max size
     */
    private static int maxInMemorySize(final int fileMaxSize) {
        if (fileMaxSize <= 0) {
            return -1;
        }
        return (int) Math.min((long) fileMaxSize * Constants.BYTES_PER_MB, Integer.MAX_VALUE);
    }

    private ServerHttpRequestDecorator decorate(final ServerWebExchange exchange,
            final CachedBodyOutputMessage outputMessage) {
        return new ServerHttpRequestDecorator(exchange.getRequest()) {
            @Override
            public Flux<DataBuffer> getBody() {
                return outputMessage.getBody();
            }
        };
    }
}
