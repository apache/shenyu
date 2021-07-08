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

package org.apache.shenyu.plugin.rewrite;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.RewriteHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.rewrite.handler.RewritePluginDataHandler;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * RewritePluginTest.
 */
@RunWith(MockitoJUnitRunner.class)
public final class RewritePluginTest {

    private RewritePlugin rewritePlugin;

    private ServerWebExchange exchange;

    @Mock
    private ShenyuPluginChain chain;

    @Before
    public void setUp() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        rewritePlugin = new RewritePlugin();
    }

    @Test
    public void testSofaPlugin() {
        RuleData data = new RuleData();
        data.setHandle("{\"rewriteURI\":\"/test\"}");
        RewriteHandle rewriteHandle = GsonUtils.getGson().fromJson(data.getHandle(), RewriteHandle.class);
        RewritePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(data), rewriteHandle);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        SelectorData selectorData = mock(SelectorData.class);
        StepVerifier.create(rewritePlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
        assertEquals("/test", exchange.getAttributes().get(Constants.REWRITE_URI));
    }

    @Test
    public void shouldReturnOriginURIForRewritePlugin() {
        RuleData data = new RuleData();
        data.setHandle("{\"rewriteURI\":\"/test\",\"regex\":\"\",\"replace\":\"\"}");
        RewriteHandle rewriteHandle = GsonUtils.getGson().fromJson(data.getHandle(), RewriteHandle.class);
        RewritePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(data), rewriteHandle);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        SelectorData selectorData = mock(SelectorData.class);
        StepVerifier.create(rewritePlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
        assertEquals("/test", exchange.getAttributes().get(Constants.REWRITE_URI));
    }

    @Test
    public void shouldReturnNewURIForRewritePlugin() {
        RuleData data = new RuleData();
        data.setHandle("{\"rewriteURI\":\"/test1\",\"regex\":\"\\\\d\",\"replace\":\"\"}");
        RewriteHandle rewriteHandle = GsonUtils.getGson().fromJson(data.getHandle(), RewriteHandle.class);
        RewritePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(data), rewriteHandle);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        SelectorData selectorData = mock(SelectorData.class);
        StepVerifier.create(rewritePlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
        assertEquals("/test", exchange.getAttributes().get(Constants.REWRITE_URI));
    }
}
