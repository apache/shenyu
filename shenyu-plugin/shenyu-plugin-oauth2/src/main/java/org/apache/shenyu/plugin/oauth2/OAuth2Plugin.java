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

package org.apache.shenyu.plugin.oauth2;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * The OAuth2Plugin.
 */
public class OAuth2Plugin implements ShenyuPlugin {

    private static final String BEARER = "Bearer ";

    private final ReactiveOAuth2AuthorizedClientService authorizedClientService;

    public OAuth2Plugin(final ReactiveOAuth2AuthorizedClientService authorizedClientService) {
        this.authorizedClientService = authorizedClientService;
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        Boolean skip = Objects.requireNonNull(exchange.<Boolean>getAttribute("skip"));
        return skip ? chain.execute(exchange)
            : exchange.getPrincipal()
            .filter(t -> t instanceof OAuth2AuthenticationToken)
            .cast(OAuth2AuthenticationToken.class)
            .flatMap(token ->
                authorizedClientService.<OAuth2AuthorizedClient>loadAuthorizedClient(token.getAuthorizedClientRegistrationId(), token.getName())
            )
            .flatMap(client -> chain.execute(this.handleToken(exchange, client)));
    }

    @Override
    public int getOrder() {
        return PluginEnum.OAUTH2.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.OAUTH2.getName();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return !Objects.requireNonNull(exchange.<Boolean>getAttribute("enable"));
    }

    private ServerWebExchange handleToken(final ServerWebExchange exchange, final OAuth2AuthorizedClient client) {
        ServerHttpRequest.Builder mutate = exchange.getRequest().mutate();
        mutate.header(HttpHeaders.AUTHORIZATION, BEARER + client.getAccessToken().getTokenValue());
        return exchange.mutate().request(mutate.build()).build();
    }
}

