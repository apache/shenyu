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

package org.apache.shenyu.plugin.sign;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.sign.api.SignService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * SignPlugin test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class SignPluginTest {
    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private SignPlugin signPlugin;

    @BeforeEach
    public void setup() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        SignService signService = mock(SignService.class);
        when(signService.signVerify(exchange)).thenReturn(Pair.of(true, ""));
        this.signPlugin = new SignPlugin(signService);
    }

    @Test
    public void testSignPlugin() {
        RuleData data = mock(RuleData.class);
        SelectorData selectorData = mock(SelectorData.class);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(signPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
    }
}
