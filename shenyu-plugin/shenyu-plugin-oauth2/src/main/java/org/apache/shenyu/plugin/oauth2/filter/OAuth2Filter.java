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

package org.apache.shenyu.plugin.oauth2.filter;

import io.netty.buffer.ByteBufAllocator;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.utils.GsonUtils;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.security.core.context.ReactiveSecurityContextHolder;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * The OAuth2Filter.
 */
@Slf4j
public class OAuth2Filter implements WebFilter {

    private final DataBufferFactory dataBufferFactory = new NettyDataBufferFactory(ByteBufAllocator.DEFAULT);

    private final ReactiveOAuth2AuthorizedClientService authorizedClientService;

    public OAuth2Filter(final ReactiveOAuth2AuthorizedClientService clientService) {
        authorizedClientService = clientService;
    }

    @Override
    public Mono<Void> filter(final ServerWebExchange serverWebExchange, final WebFilterChain webFilterChain) {

        Boolean enable = (Boolean) serverWebExchange.getAttributes().get("enable");
        if (!enable) {
            return webFilterChain.filter(serverWebExchange);
        }
        return ReactiveSecurityContextHolder.getContext()
            .map(SecurityContext::getAuthentication)
            .filter(t -> t instanceof OAuth2AuthenticationToken)
            .cast(OAuth2AuthenticationToken.class)
            .<OAuth2AuthorizedClient>flatMap(token -> {
                        Map<String, Object> userInfo = token.getPrincipal().getAttributes();
                        serverWebExchange.getAttributes().put("UserInfo", userInfo);
                        return authorizedClientService.loadAuthorizedClient(token.getAuthorizedClientRegistrationId(), token.getName());
                    }
            )
            .flatMap(client -> {
                serverWebExchange.getAttributes().put("Authorization", client.getAccessToken());
                serverWebExchange.getAttributes().put("Principal", client.getPrincipalName());
                return Mono.empty();
            })
            .thenEmpty(webFilterChain.filter(this.addTokenToHeader(serverWebExchange)));
    }

    /**
     * Modify ServerWebExchange.
     *
     * @param exchange The exchange
     * @return New ServerWebExchange Instance
     */
    private ServerWebExchange addTokenToHeader(final ServerWebExchange exchange) {
        ServerHttpRequest.Builder requestMutate = exchange.getRequest().mutate();
        requestMutate.header("Principal", exchange.<String>getAttribute("Principal"));
        requestMutate.header("Authorization", exchange.<String>getAttribute("Authorization"));
        ServerWebExchange build = exchange.mutate().request(requestMutate.build()).build();
        return addUserInfoToBody(build);
    }

    private ServerWebExchange addUserInfoToBody(final ServerWebExchange exchange) {
        ServerHttpRequestDecorator decorator = new ServerHttpRequestDecorator(exchange.getRequest()) {
            @Override
            public Flux<DataBuffer> getBody() {
                Map<String, Object> userInfo = exchange.getAttribute("UserInfo");
                if (userInfo == null) {
                    return super.getBody();
                }
                Flux<DataBuffer> bodyBuffer = super.getBody();

                return bodyBuffer.map(buf -> {
                    int readableByteCount = buf.readableByteCount();
                    if (readableByteCount > 0) {
                        return appendBody(buf, userInfo);
                    }
                    String json = GsonUtils.getInstance().toJson(userInfo);
                    buf.write(json.getBytes(StandardCharsets.UTF_8));
                    return buf;
                });
            }
        };
        return exchange.mutate().request(decorator).build();
    }

    private DataBuffer appendBody(final DataBuffer buf, final Map<String, Object> userInfo) {
        List<Object> newBody = new ArrayList<>();
        byte[] reads = new byte[buf.readableByteCount()];
        buf.read(reads);
        Map<String, Object> bodyMap = GsonUtils.getInstance().convertToMap(new String(reads));
        newBody.add(bodyMap);
        newBody.add(userInfo);
        DataBuffer dataBuffer = dataBufferFactory.allocateBuffer();
        String newBodyJson = GsonUtils.getInstance().toJson(newBody);
        dataBuffer.write(newBodyJson.getBytes(StandardCharsets.UTF_8));
        return dataBuffer;
    }
}
