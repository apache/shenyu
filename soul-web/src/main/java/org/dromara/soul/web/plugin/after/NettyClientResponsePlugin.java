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

package org.dromara.soul.web.plugin.after;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
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
public class NettyClientResponsePlugin implements SoulPlugin {

    private static final Log LOG = LogFactory.getLog(NettyClientResponsePlugin.class);

    private List<MediaType> streamingMediaTypes = Arrays
            .asList(MediaType.TEXT_EVENT_STREAM, MediaType.APPLICATION_STREAM_JSON);

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final SoulPluginChain chain) {
        return Mono.defer(() -> {
            final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
            assert requestDTO != null;
            Connection connection = exchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR);
            if (connection == null) {
                return Mono.empty();
            }
            if (LOG.isTraceEnabled()) {
                LOG.trace("NettyWriteResponseFilter start inbound: "
                        + connection.channel().id().asShortText() + ", outbound: "
                        + exchange.getLogPrefix());
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

        })
                .then(chain.execute(exchange)
                        .doOnError(throwable -> cleanup(exchange))).doOnCancel(() -> cleanup(exchange));
    }

    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.LAST;
    }

    @Override
    public int getOrder() {
        return 100;
    }

    @Override
    public String named() {
        return "NettyWriteResponse";
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
        assert requestDTO != null;
        return !Objects.equals(RpcTypeEnum.HTTP.getName(), requestDTO.getRpcType())
                && !Objects.equals(RpcTypeEnum.SPRING_CLOUD.getName(), requestDTO.getRpcType());
    }

    private void cleanup(final ServerWebExchange exchange) {
        Connection connection = exchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR);
        if (connection != null) {
            connection.dispose();
        }
    }

    private boolean isStreamingMediaType(@Nullable final MediaType contentType) {
        return contentType != null && this.streamingMediaTypes.stream()
                .anyMatch(contentType::isCompatibleWith);
    }

}
