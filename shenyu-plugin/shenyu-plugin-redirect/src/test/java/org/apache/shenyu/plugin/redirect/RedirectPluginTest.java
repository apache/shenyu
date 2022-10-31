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

package org.apache.shenyu.plugin.redirect;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.RedirectHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.redirect.handler.RedirectPluginDataHandler;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test case for {@link RedirectPlugin}.
 */
@ExtendWith(MockitoExtension.class)
public final class RedirectPluginTest {

    private RedirectPlugin redirectPlugin;

    private ServerWebExchange exchange;

    @Mock
    private ShenyuPluginChain chain;

    @Mock
    private DispatcherHandler dispatcherHandler;

    @BeforeEach
    public void setUp() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        redirectPlugin = new RedirectPlugin(dispatcherHandler);
    }

    @Test
    public void testRedirectPlugin() {
        RuleData ruleData = new RuleData();
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        SelectorData selectorData = mock(SelectorData.class);
        StepVerifier.create(redirectPlugin.doExecute(exchange, chain, selectorData, ruleData)).expectSubscription().verifyComplete();
        ruleData.setHandle("{\"redirectURI\":\"/test\"}");
        RedirectHandle redirectHandle = new RedirectHandle();
        redirectHandle.setRedirectURI("/test");
        RedirectPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), redirectHandle);
        when(dispatcherHandler.handle(any())).thenReturn(Mono.empty());
        StepVerifier.create(redirectPlugin.doExecute(exchange, chain, selectorData, ruleData)).expectSubscription().verifyComplete();
        redirectHandle.setRedirectURI("http://test.com/test");
        RedirectPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), redirectHandle);
        ruleData.setHandle("{\"redirectURI\":\"http://test.com/test\"}");
        StepVerifier.create(redirectPlugin.doExecute(exchange, chain, selectorData, ruleData)).expectSubscription().verifyComplete();
    }

    @Test
    public void testGetOrder() {
        final int result = redirectPlugin.getOrder();
        assertThat(PluginEnum.REDIRECT.getCode(), Matchers.is(result));
    }

    @Test
    public void testNamed() {
        final String result = redirectPlugin.named();
        assertThat(PluginEnum.REDIRECT.getName(), Matchers.is(result));
    }

}
