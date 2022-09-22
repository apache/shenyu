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
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.lang.Nullable;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.adapter.DefaultServerWebExchange;
import org.springframework.web.server.i18n.AcceptHeaderLocaleContextResolver;
import org.springframework.web.server.session.DefaultWebSessionManager;
import org.springframework.web.server.session.WebSessionManager;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.security.Principal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class OAuth2PluginTest {

    private OAuth2Plugin oAuth2Plugin;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private ReactiveOAuth2AuthorizedClientService reactiveOAuth2AuthorizedClientService;

    @BeforeEach
    public void setup() {
        exchange = MockServerWebExchange2.from(MockServerHttpRequest.get("localhost").build());
        exchange.getAttributes().put("skip", true);
        chain = mock(ShenyuPluginChain.class, (Answer<Mono>) invocationOnMock -> Mono.empty());
        reactiveOAuth2AuthorizedClientService = mock(ReactiveOAuth2AuthorizedClientService.class);
        this.oAuth2Plugin = new OAuth2Plugin(reactiveOAuth2AuthorizedClientService);
    }

    @Test
    public void testOAuth2Plugin() {
        StepVerifier.create(oAuth2Plugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        exchange.getAttributes().put("skip", false);
        final OAuth2AuthorizedClient oAuth2AuthorizedClient = mock(OAuth2AuthorizedClient.class);

        when(reactiveOAuth2AuthorizedClientService.loadAuthorizedClient(null, null)).thenReturn(Mono.just(oAuth2AuthorizedClient));
        final OAuth2AccessToken oAuth2AccessToken = mock(OAuth2AccessToken.class);
        when(oAuth2AuthorizedClient.getAccessToken()).thenReturn(oAuth2AccessToken);
        when(oAuth2AccessToken.getTokenValue()).thenReturn("tokenValue");
        StepVerifier.create(oAuth2Plugin.execute(exchange, chain)).expectSubscription().verifyComplete();
    }

    @Test
    public void skipTest() {
        Assertions.assertTrue(this.oAuth2Plugin.skip(exchange));
    }

    @Test
    public void namedTest() {
        assertEquals(PluginEnum.OAUTH2.getName(), oAuth2Plugin.named());
    }

    @Test
    public void getOrderTest() {
        assertEquals(PluginEnum.OAUTH2.getCode(), oAuth2Plugin.getOrder());
    }

    public static final class MockServerWebExchange2 extends DefaultServerWebExchange {

        private final OAuth2AuthenticationToken oAuth2AuthenticationToken = mock(OAuth2AuthenticationToken.class);

        private MockServerWebExchange2(final MockServerHttpRequest request, final WebSessionManager sessionManager) {
            super(request, new MockServerHttpResponse(), sessionManager, ServerCodecConfigurer.create(), new AcceptHeaderLocaleContextResolver());
        }

        /**
        * from.
        * @param request request
        * @return MockServerWebExchange2
        */
        public static MockServerWebExchange2 from(final MockServerHttpRequest request) {
            return builder(request).build();
        }

        /**
        * builder.
        * @param request request
        * @return MockServerWebExchange2.Builder
        */
        public static MockServerWebExchange2.Builder builder(final MockServerHttpRequest request) {
            return new MockServerWebExchange2.Builder(request);
        }

        /**
        * getPrincipal.
        * @param <T> t
        * @return Principal
        */
        public <T extends Principal> Mono<T> getPrincipal() {
            return (Mono<T>) Mono.just(oAuth2AuthenticationToken);
        }

        public static class Builder {
            private final MockServerHttpRequest request;

            @Nullable
            private WebSessionManager sessionManager;

            public Builder(final MockServerHttpRequest request) {
                this.request = request;
            }

            /**
            * build.
            * @return MockServerWebExchange2
            */
            public MockServerWebExchange2 build() {
                return new MockServerWebExchange2(this.request, this.sessionManager != null ? this.sessionManager : new DefaultWebSessionManager());
            }
        }
    }
}

