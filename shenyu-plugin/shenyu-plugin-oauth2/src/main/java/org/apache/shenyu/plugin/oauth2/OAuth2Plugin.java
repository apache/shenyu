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

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * The OAuth2 Plugin.
 */
public class OAuth2Plugin extends AbstractShenyuPlugin {

    private static final String BEARER = "Bearer ";

    private final ObjectProvider<ReactiveOAuth2AuthorizedClientService> authorizedClientServiceProvider;
    
    /**
     * Instantiates a new oauth2 plugin.
     *
     * @param authorizedClientServiceProvider the authorized client service provider
     */
    public OAuth2Plugin(final ObjectProvider<ReactiveOAuth2AuthorizedClientService> authorizedClientServiceProvider) {
        this.authorizedClientServiceProvider = authorizedClientServiceProvider;
    }
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        return exchange.getPrincipal()
                .filter(OAuth2AuthenticationToken.class::isInstance)
                .cast(OAuth2AuthenticationToken.class)
                .flatMap(this::buildAuthorizedClient)
                .flatMap(client -> chain.execute(this.writeToken(exchange, client)));
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
        return skipExceptHttpLike(exchange);
    }
    
    private Mono<OAuth2AuthorizedClient> buildAuthorizedClient(final OAuth2AuthenticationToken oauth2Authentication) {
        String clientRegistrationId = oauth2Authentication.getAuthorizedClientRegistrationId();
        String name = oauth2Authentication.getName();
        ReactiveOAuth2AuthorizedClientService clientService = authorizedClientServiceProvider.getIfAvailable();
        if (Objects.isNull(clientService)) {
            return Mono.error(new IllegalStateException(
                    "ReactiveOAuth2AuthorizedClientService bean was found. you have to add "
                            + " spring-boot-starter-oauth2-client dependency?"));
        }
        return clientService.loadAuthorizedClient(clientRegistrationId, name);
    }

    private ServerWebExchange writeToken(final ServerWebExchange exchange, final OAuth2AuthorizedClient client) {
        ServerHttpRequest.Builder mutate = exchange.getRequest().mutate();
        mutate.header(HttpHeaders.AUTHORIZATION, BEARER + client.getAccessToken().getTokenValue());
        return exchange.mutate().request(mutate.build()).build();
    }
}

