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

package org.apache.shenyu.plugin.request;

import com.google.common.collect.ImmutableMap;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle.ShenyuCookie;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle.ShenyuRequestHeader;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle.ShenyuRequestParameter;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.request.handler.RequestPluginHandler;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.internal.util.collections.Sets;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

/**
 * Request plugin test.
 */
@RunWith(MockitoJUnitRunner.class)
public class RequestPluginTest {
    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private RequestPlugin requestPlugin;

    private RuleData ruleData;

    @Before
    public void setup() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .cookie(new HttpCookie("replaceKey", "oldValue"))
                .cookie(new HttpCookie("removeKey", "value"))
                .cookie(new HttpCookie("setKey", "oldValue"))
                .header("replaceKey", "oldValue")
                .header("removeKey", "value")
                .header("setKey", "oldValue")
                .queryParam("replaceKey", "oldValue")
                .queryParam("removeKey", "value")
                .queryParam("setKey", "oldValue")
                .build());
        this.requestPlugin = new RequestPlugin();
        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test-selectorId");
        this.ruleData.setName("test-request-plugin");

        RequestHandle requestHandle = new RequestHandle();
        ShenyuRequestHeader requestHeader = requestHandle.new ShenyuRequestHeader(
                ImmutableMap.of("addKey", "addValue"), ImmutableMap.of("replaceKey", "newKey"),
                ImmutableMap.of("setKey", "newValue"), Sets.newSet("removeKey")
        );

        ShenyuRequestParameter requestParameter = requestHandle.new ShenyuRequestParameter(
                ImmutableMap.of("addKey", "addValue"), ImmutableMap.of("replaceKey", "newKey"),
                ImmutableMap.of("setKey", "newValue"), Sets.newSet("removeKey")
        );
        ShenyuCookie cookie = requestHandle.new ShenyuCookie(
                ImmutableMap.of("addKey", "addValue"), ImmutableMap.of("replaceKey", "newKey"),
                ImmutableMap.of("setKey", "newValue"), Sets.newSet("removeKey")
        );
        requestHandle.setHeader(requestHeader);
        requestHandle.setParameter(requestParameter);
        requestHandle.setCookie(cookie);

        RequestPluginHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(this.ruleData), requestHandle);
    }

    @Test
    public void testDoExecute() {
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());

        StepVerifier.create(requestPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();

        ArgumentCaptor<ServerWebExchange> newExchange = ArgumentCaptor.forClass(ServerWebExchange.class);
        Mockito.verify(this.chain, times(1)).execute(newExchange.capture());

        ServerHttpRequest request = newExchange.getValue().getRequest();
        assertNotNull(request);
        HttpHeaders httpHeaders = request.getHeaders();
        assertNotNull(httpHeaders);
        assertNotNull(httpHeaders.containsKey("addKey"));
        assertNotNull(httpHeaders.get("addKey").size() == 1 && "addValue".equals(httpHeaders.get("addKey")));
        assertNotNull(httpHeaders.containsKey("newKey"));
        assertNotNull(httpHeaders.get("newKey").size() == 1 && "oldValue".equals(httpHeaders.get("newKey")));
        assertNotNull(httpHeaders.containsKey("setKey"));
        assertNotNull(httpHeaders.get("setKey").size() == 1 && "newValue".equals(httpHeaders.get("setKey")));
        assertFalse(httpHeaders.containsKey("removeKey"));
        assertTrue(httpHeaders.containsKey(HttpHeaders.COOKIE));

        MultiValueMap<String, String> queryParams = request.getQueryParams();
        assertNotNull(queryParams);
        assertNotNull(queryParams.containsKey("addKey"));
        assertNotNull(queryParams.get("addKey").size() == 1 && "addValue".equals(queryParams.get("addKey")));
        assertNotNull(queryParams.containsKey("newKey"));
        assertNotNull(queryParams.get("newKey").size() == 1 && "oldValue".equals(queryParams.get("newKey")));
        assertNotNull(queryParams.containsKey("setKey"));
        assertNotNull(queryParams.get("setKey").size() == 1 && "newValue".equals(queryParams.get("setKey")));
        assertFalse(queryParams.containsKey("removeKey"));
    }

    @Test
    public void testGetOrder() {
        assertEquals(this.requestPlugin.getOrder(), PluginEnum.REQUEST.getCode());
    }

    @Test
    public void tesNamed() {
        assertEquals(this.requestPlugin.named(), PluginEnum.REQUEST.getName());
    }
}
