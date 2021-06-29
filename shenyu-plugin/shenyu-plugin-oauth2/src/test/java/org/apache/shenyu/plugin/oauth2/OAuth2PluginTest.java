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
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

@RunWith(MockitoJUnitRunner.class)
public class OAuth2PluginTest {

    private OAuth2Plugin oAuth2Plugin;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    @Before
    public void setup() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        exchange.getAttributes().put("skip", true);
        chain = mock(ShenyuPluginChain.class, (Answer<Mono>) invocationOnMock -> Mono.empty());
        this.oAuth2Plugin = new OAuth2Plugin(mock(ReactiveOAuth2AuthorizedClientService.class));
    }

    @Test
    public void testOAuth2Plugin() {
        StepVerifier.create(oAuth2Plugin.execute(exchange, chain)).expectSubscription().verifyComplete();
    }

    @Test
    public void namedTest() {
        assertEquals(PluginEnum.OAUTH2.getName(), oAuth2Plugin.named());
    }

    @Test
    public void getOrderTest() {
        assertEquals(PluginEnum.OAUTH2.getCode(), oAuth2Plugin.getOrder());
    }

}

