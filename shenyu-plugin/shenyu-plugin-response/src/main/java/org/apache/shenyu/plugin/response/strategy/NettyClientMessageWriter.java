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

package org.apache.shenyu.plugin.response.strategy;

import com.google.common.collect.Lists;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.lang.Nullable;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * The type Netty client message writer.
 */
public class NettyClientMessageWriter implements MessageWriter {

    /**
     * stream media type: from APPLICATION_STREAM_JSON upgrade to APPLICATION_STREAM_JSON_VALUE.
     * Both of the above have expired.
     * latest version: {@linkplain MediaType#APPLICATION_NDJSON}
     */
    private final List<MediaType> streamingMediaTypes = Arrays.asList(MediaType.TEXT_EVENT_STREAM, MediaType.APPLICATION_NDJSON);

    @Override
    public Mono<Void> writeWith(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange).doOnError(throwable -> cleanup(exchange)).then(Mono.defer(() -> {
            Connection connection = exchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR);
            if (Objects.isNull(connection)) {
                return Mono.empty();
            }
            ServerHttpResponse response = exchange.getResponse();
            NettyDataBufferFactory factory = (NettyDataBufferFactory) response.bufferFactory();
            final Flux<NettyDataBuffer> body = connection
                    .inbound()
                    .receive()
                    .retain()
                    .map(factory::wrap);
            MediaType contentType = response.getHeaders().getContentType();

            Mono<Void> responseMono = isStreamingMediaType(contentType)
                    ? response.writeAndFlushWith(body.map(Flux::just))
                    : response.writeWith(body);
            exchange.getAttributes().put(Constants.RESPONSE_MONO, responseMono);
            // watcher httpStatus
            final Consumer<HttpStatus> consumer = exchange.getAttribute(Constants.WATCHER_HTTP_STATUS);
            Optional.ofNullable(consumer).ifPresent(c -> c.accept(response.getStatusCode()));
            return responseMono;
        })).doOnCancel(() -> cleanup(exchange));
    }
    
    @Override
    public List<String> supportTypes() {
        return Lists.newArrayList(RpcTypeEnum.HTTP.getName(), RpcTypeEnum.SPRING_CLOUD.getName());
    }
    
    private void cleanup(final ServerWebExchange exchange) {
        Connection connection = exchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR);
        if (Objects.nonNull(connection)) {
            connection.dispose();
        }
    }

    private boolean isStreamingMediaType(@Nullable final MediaType contentType) {
        return Objects.nonNull(contentType) && this.streamingMediaTypes.stream().anyMatch(contentType::isCompatibleWith);
    }
}
