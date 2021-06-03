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

package org.apache.shenyu.web.filter;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import java.util.Collections;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for WebSocketParamFilter.
 */
@RunWith(MockitoJUnitRunner.class)
public final class WebSocketParamFilterTest<T> extends WebSocketParamFilter {

    private static final String UPGRADE = "Upgrade";

    @Mock
    private ShenyuResult<T> shenyuResult;

    @Mock
    private WebFilterChain chain;

    @Test
    public void testDoFilter() {
        MultiValueMap<String, String> headerParams = new LinkedMultiValueMap<>(1);
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>(3);
        verifyTrue(getResult(headerParams, queryParams));

        headerParams.put(UPGRADE, Collections.singletonList(RpcTypeEnum.HTTP.getName()));
        verifyTrue(getResult(headerParams, queryParams));

        headerParams.put(UPGRADE, Collections.singletonList(RpcTypeEnum.WEB_SOCKET.getName()));
        verifyFalse(getResult(headerParams, queryParams));

        queryParams.put(Constants.MODULE, Collections.singletonList("module_placeholder"));
        verifyFalse(getResult(headerParams, queryParams));

        queryParams.put(Constants.METHOD, Collections.singletonList("mothod_placeholder"));
        verifyFalse(getResult(headerParams, queryParams));

        queryParams.put(Constants.RPC_TYPE, Collections.singletonList("rpc_type_palceholder"));
        verifyTrue(getResult(headerParams, queryParams));
    }

    @Test
    public void testDoDenyResponse() {
        final ConfigurableApplicationContext cfgContext = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setCfgContext(cfgContext);
        when(SpringBeanUtils.getInstance().getBean(ShenyuResult.class)).thenReturn(shenyuResult);
        final Mono<Void> result = doDenyResponse(createExchange(new LinkedMultiValueMap<>(1), new LinkedMultiValueMap<>(1)));
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
    
    private Mono<Boolean> getResult(final MultiValueMap<String, String> headerParams, final MultiValueMap<String, String> queryParams) {
        return doFilter(createExchange(headerParams, queryParams), chain);
    }
    
    private ServerWebExchange createExchange(final MultiValueMap<String, String> headerParams, final MultiValueMap<String, String> queryParams) {
        return MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .headers(headerParams)
                .queryParams(queryParams)
                .build());
    }
    
    private void verifyTrue(final Mono<Boolean> result) {
        StepVerifier.create(result).expectSubscription().expectNext(Boolean.TRUE).verifyComplete();
    }
    
    private void verifyFalse(final Mono<Boolean> result) {
        StepVerifier.create(result).expectSubscription().expectNext(Boolean.FALSE).verifyComplete();
    }
}
