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
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.security.core.context.ReactiveSecurityContextHolder;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.OAuth2RefreshToken;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The OAuth2Filter.
 */
@Slf4j
public class OAuth2Filter implements WebFilter {

    private static final AtomicBoolean REFRESH_TOKEN = new AtomicBoolean(false);

    private final DataBufferFactory dataBufferFactory = new NettyDataBufferFactory(ByteBufAllocator.DEFAULT);

    private final ReactiveOAuth2AuthorizedClientService authorizedClientService;

    public OAuth2Filter(final ReactiveOAuth2AuthorizedClientService clientService) {
        authorizedClientService = clientService;
    }

    @Override
    public Mono<Void> filter(final ServerWebExchange serverWebExchange, final WebFilterChain webFilterChain) {

        Boolean enable = (Boolean) serverWebExchange.getAttributes().get("enable");
        if (!enable || !REFRESH_TOKEN.get()) {
            return webFilterChain.filter(serverWebExchange);
        }
        return ReactiveSecurityContextHolder.getContext()
            .map(SecurityContext::getAuthentication)
            .filter(t -> t instanceof OAuth2AuthenticationToken)
            .cast(OAuth2AuthenticationToken.class)
            .flatMap(token ->
                authorizedClientService.loadAuthorizedClient(token.getAuthorizedClientRegistrationId(), token.getName())
                    .flatMap(client -> this.handleToken(serverWebExchange, client, token.getPrincipal().getAttributes()))
                    .flatMap(webFilterChain::filter)
            );
    }

    /**
     * process Token.
     *
     * @param exchange The exchange
     * @return New ServerWebExchange Instance
     */
    private Mono<ServerWebExchange> handleToken(final ServerWebExchange exchange, final OAuth2AuthorizedClient client, final Map<String, Object> userInfoMap) {
        ServerHttpRequestDecorator decorator = new ServerHttpRequestDecorator(exchange.getRequest()) {
            @Override
            public Flux<DataBuffer> getBody() {
                return Flux.just(dataBufferFactory.allocateBuffer())
                    .map(buf -> {
                        List<Object> res = new ArrayList<>();

                        OAuth2RefreshToken refreshToken = client.getRefreshToken();
                        OAuth2AccessToken accessToken = client.getAccessToken();
                        Map<String, Object> tokenMap = handleTokenProperty(refreshToken, accessToken);

                        res.add(userInfoMap);
                        res.add(tokenMap);
                        handleOriginBody(super.getBody(), res);

                        buf.write(GsonUtils.getInstance().toJson(res).getBytes(StandardCharsets.UTF_8));
                        return buf;
                    });
            }
        };
        return Mono.just(exchange.mutate().request(decorator).build());
    }

    private Map<String, Object> handleTokenProperty(final OAuth2RefreshToken refreshToken, final OAuth2AccessToken accessToken) {
        Map<String, Object> tokenMap = new HashMap<>();
        if (Objects.nonNull(refreshToken)) {
            tokenMap.put("refresh_token", refreshToken.getTokenValue());
        }
        if (Objects.isNull(accessToken)) {
            return tokenMap;
        }
        Instant expiresAt = Objects.requireNonNull(accessToken.getExpiresAt());
        Instant issuedAt = Objects.requireNonNull(accessToken.getIssuedAt());
        // todo maintain refresh token status
        long expiresIn = expiresAt.minusSeconds(issuedAt.getEpochSecond()).getEpochSecond();
        if (expiresIn > 1) {
            tokenMap.put("expires_in", expiresIn);
            REFRESH_TOKEN.set(false);
        }
        if (tokenMap.containsKey("refresh_token") && expiresAt.getEpochSecond() < Instant.now().getEpochSecond()) {
            while (!REFRESH_TOKEN.compareAndSet(false, true)) {
                log.info("");
            }
        }

        tokenMap.put("access_token", accessToken.getTokenValue());
        tokenMap.put("token_type", accessToken.getTokenType());
        tokenMap.put("scope", accessToken.getScopes());
        return tokenMap;
    }

    private void handleOriginBody(final Flux<DataBuffer> body, final List<Object> res) {
        body.map(data -> {
            byte[] b = new byte[data.readableByteCount()];
            data.read(b);
            return new String(b);
        }).subscribe(res::add);
    }
}
