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

import org.apache.shenyu.common.utils.ShaUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test case for LocalDispatcherFilter.
 * @see LocalDispatcherFilter
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class LocalDispatcherFilterTest {
    private LocalDispatcherFilter localDispatcherFilter;

    private WebFilterChain webFilterChain;

    @BeforeEach
    public void setup() {
        DispatcherHandler dispatcherHandler = mock(DispatcherHandler.class);
        when(dispatcherHandler.handle(any())).thenReturn(Mono.empty());
        String sha512Key = ShaUtils.shaEncryption("123456");
        localDispatcherFilter = new LocalDispatcherFilter(dispatcherHandler, sha512Key);
        webFilterChain = mock(WebFilterChain.class);
        when(webFilterChain.filter(any())).thenReturn(Mono.empty());
    }

    @Test
    public void testFilterMatch() {
        ServerWebExchange exchangeNormal =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/shenyu/test")
                        .header("localKey", "123456"));
        Mono<Void> filterNormal = localDispatcherFilter.filter(exchangeNormal, webFilterChain);
        StepVerifier.create(filterNormal).expectSubscription().verifyComplete();
    }

    @Test
    public void testFilterNotMatch() {
        ServerWebExchange noHeaderExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/shenyu/test"));
        Mono<Void> noHeaderFilter = localDispatcherFilter.filter(noHeaderExchange, webFilterChain);
        StepVerifier.create(noHeaderFilter).expectSubscription().verifyError(ResponseStatusException.class);

        ServerWebExchange noMatchUrlExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/test")
                        .header("localKey", "123456"));
        Mono<Void> noMatchUrlFilter = localDispatcherFilter.filter(noMatchUrlExchange, webFilterChain);
        StepVerifier.create(noMatchUrlFilter).expectSubscription().verifyComplete();

        ServerWebExchange shaErrorExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/shenyu/test")
                        .header("localKey", "654321"));
        Mono<Void> shaErrorFilter = localDispatcherFilter.filter(shaErrorExchange, webFilterChain);
        StepVerifier.create(shaErrorFilter).expectSubscription().verifyError(ResponseStatusException.class);
    }
}
