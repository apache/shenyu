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

package org.dromara.soul.plugin.httpclient.response;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPlugin;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
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

/**
 * The type Netty client response plugin.
 *
 * @author xiaoyu
 */
@Slf4j
public class NettyClientResponsePlugin implements SoulPlugin {

    private final List<MediaType> streamingMediaTypes = Arrays.asList(MediaType.TEXT_EVENT_STREAM, MediaType.APPLICATION_STREAM_JSON);

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final SoulPluginChain chain) {
        return chain.execute(exchange).doOnError(throwable -> cleanup(exchange)).then(Mono.defer(() -> {
            Connection connection = exchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR);
            if (connection == null) {
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
            return isStreamingMediaType(contentType)
                    ? response.writeAndFlushWith(body.map(Flux::just))
                    : response.writeWith(body);
        })).doOnCancel(() -> cleanup(exchange));
    }

    @Override
    public int getOrder() {
        return 100;
    }

    @Override
    public String named() {
        return "NettyClientResponse";
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        return !Objects.equals(RpcTypeEnum.HTTP.getName(), soulContext.getRpcType())
                && !Objects.equals(RpcTypeEnum.SPRING_CLOUD.getName(), soulContext.getRpcType());
    }

    private void cleanup(final ServerWebExchange exchange) {
        Connection connection = exchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR);
        if (connection != null) {
            connection.dispose();
        }
    }

    private boolean isStreamingMediaType(@Nullable final MediaType contentType) {
        return contentType != null && this.streamingMediaTypes.stream().anyMatch(contentType::isCompatibleWith);
    }
}
