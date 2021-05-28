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

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.security.core.context.ReactiveSecurityContextHolder;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

/**
 * The OAuth2Filter.
 */
@Slf4j
public class OAuth2Filter implements WebFilter {

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
            .<OAuth2AuthorizedClient>flatMap(token ->
                authorizedClientService.loadAuthorizedClient(token.getAuthorizedClientRegistrationId(), token.getName())
            )
            .flatMap(t -> {
                serverWebExchange.getAttributes().put("principal", t.getPrincipalName());
                return Mono.empty();
            }).thenEmpty(webFilterChain.filter(this.addTokenToHeader(serverWebExchange)));
    }

    /**
     * Modify ServerWebExchange.
     *
     * @param exchange The exchange
     * @return New ServerWebExchange Instance
     */
    private ServerWebExchange addTokenToHeader(final ServerWebExchange exchange) {
        ServerHttpRequest.Builder requestMutate = exchange.getRequest().mutate();
        requestMutate.header("principal", exchange.<String>getAttribute("principal"));

        return exchange.mutate().request(requestMutate.build()).build();
    }
}
