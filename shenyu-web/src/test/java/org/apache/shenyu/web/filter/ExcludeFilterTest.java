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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ExcludeFilterTest {

    private ExcludeFilter excludeFilter;

    private WebFilterChain webFilterChain;

    @BeforeEach
    public void setUp() {
        List<String> paths = new ArrayList<>();
        paths.add("/favicon.ico");
        excludeFilter = new ExcludeFilter(paths);
        webFilterChain = mock(WebFilterChain.class);
        when(webFilterChain.filter(any())).thenReturn(Mono.empty());
    }

    @Test
    public void testDoMatcher() {
        ServerWebExchange webExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/favicon.ico"));
        Mono<Boolean> filter = excludeFilter.doMatcher(webExchange, webFilterChain);
        StepVerifier.create(filter).expectNext(Boolean.TRUE).verifyComplete();
    }

    @Test
    public void testDoNotMatcher() {
        ServerWebExchange webExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/"));
        Mono<Boolean> filter = excludeFilter.doMatcher(webExchange, webFilterChain);
        StepVerifier.create(filter).expectNext(Boolean.FALSE).verifyComplete();
    }

    @Test
    public void testDoFilter() {
        ServerWebExchange webExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/"));
        Mono<Void> filter = excludeFilter.doFilter(webExchange);
        StepVerifier.create(filter).verifyComplete();
    }

}
