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

import org.apache.shenyu.web.configuration.properties.ExcludePathProperties;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Arrays;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.Silent.class)
public class ExcludeFilterTest {

    @Mock
    private ExcludePathProperties mockExcludePathProperties;

    private ExcludeFilter excludeFilter;

    private WebFilterChain webFilterChain;

    @Before
    public void setUp() {
        excludeFilter = new ExcludeFilter(mockExcludePathProperties);
        webFilterChain = mock(WebFilterChain.class);
        when(webFilterChain.filter(any())).thenReturn(Mono.empty());
        when(mockExcludePathProperties.getPaths()).thenReturn(Arrays.asList("/favicon.ico"));
    }

    @Test
    public void testFilterMatch() {
        ServerWebExchange webExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/favicon.ico"));
        Mono<Void> filter = excludeFilter.filter(webExchange, webFilterChain);
        StepVerifier.create(filter).expectSubscription().verifyComplete();
    }

    @Test
    public void testFilterNotMatch() {
        ServerWebExchange webExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/"));
        Mono<Void> filter = excludeFilter.filter(webExchange, webFilterChain);
        StepVerifier.create(filter).expectSubscription().verifyComplete();
    }
}
