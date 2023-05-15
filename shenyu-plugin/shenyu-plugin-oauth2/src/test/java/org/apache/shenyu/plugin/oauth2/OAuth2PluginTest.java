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

import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.security.core.context.SecurityContextImpl;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.security.web.server.context.SecurityContextServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Collections;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class OAuth2PluginTest {
    
    private static final Duration TIMEOUT = Duration.ofSeconds(25);

    private OAuth2Plugin oAuth2Plugin;

    private MockServerWebExchange mockExchange;
    
    private MockServerHttpRequest request;

    private ShenyuPluginChain chain;

    private ReactiveOAuth2AuthorizedClientService oAuth2AuthorizedClientService;

    @BeforeEach
    public void setup() {
        request = MockServerHttpRequest.get("/hello-world").build();
        mockExchange = MockServerWebExchange.from(request);
        chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());
        oAuth2AuthorizedClientService = mock(ReactiveOAuth2AuthorizedClientService.class);
        ObjectProvider<ReactiveOAuth2AuthorizedClientService> objectProvider = mock(ObjectProvider.class);
        when(objectProvider.getIfAvailable()).thenReturn(oAuth2AuthorizedClientService);
        this.oAuth2Plugin = new OAuth2Plugin(objectProvider);
    }
    
    @Test
    public void havePrincipal() {
        OAuth2AccessToken accessToken = mock(OAuth2AccessToken.class);
        when(accessToken.getTokenValue()).thenReturn("token");
        ClientRegistration clientRegistration = ClientRegistration.withRegistrationId("registration_id")
                .authorizationGrantType(AuthorizationGrantType.CLIENT_CREDENTIALS).clientId("client_id")
                .tokenUri("token_uri").build();
        OAuth2AuthorizedClient authorizedClient = new OAuth2AuthorizedClient(clientRegistration, "shenyu", accessToken);
        
        when(oAuth2AuthorizedClientService.loadAuthorizedClient(any(String.class), any())).thenReturn(Mono.just(authorizedClient));
        
        OAuth2AuthenticationToken authenticationToken = new OAuth2AuthenticationToken(mock(OAuth2User.class),
                Collections.emptyList(), "myId");
        SecurityContextImpl securityContext = new SecurityContextImpl(authenticationToken);
        SecurityContextServerWebExchange exchange = new SecurityContextServerWebExchange(mockExchange, Mono.just(securityContext));
    
        oAuth2Plugin.doExecute(exchange, chain, null, null).block(TIMEOUT);
    
        assertTrue(request.getHeaders().containsKey(HttpHeaders.AUTHORIZATION));
        assertTrue(Objects.requireNonNull(request.getHeaders().get(HttpHeaders.AUTHORIZATION)).contains("Bearer token"));
    }
}

