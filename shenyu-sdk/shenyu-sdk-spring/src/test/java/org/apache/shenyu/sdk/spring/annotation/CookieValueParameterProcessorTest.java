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

package org.apache.shenyu.sdk.spring.annotation;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import org.springframework.http.HttpHeaders;
import org.springframework.web.bind.annotation.CookieValue;

/**
 * {@link CookieValue} process test.
 */
public class CookieValueParameterProcessorTest {

    private ShenyuRequest request;

    private final Map<String, Collection<String>> headers = new HashMap<>();

    private AnnotatedParameterProcessor processor;

    @BeforeEach
    public void init() {
        this.processor = new CookieValueParameterProcessor();
        this.headers.clear();
        final ShenyuRequest req = mock(ShenyuRequest.class);
        final RequestTemplate template = mock(RequestTemplate.class);
        when(req.getRequestTemplate()).thenReturn(template);
        when(template.getHeaders()).thenReturn(Maps.newHashMap());
        when(req.getHeaders()).thenReturn(headers);
        this.request = req;
    }

    @Test
    public void processArgumentNullTest() {
        final CookieValue cookie = spy(CookieValue.class);
        when(cookie.value()).thenReturn("");
        Assert.assertThrows(IllegalStateException.class, () -> processor.processArgument(request, cookie, ""));
    }

    @Test
    public void processArgumentOneTest() {
        final CookieValue cookie = spy(CookieValue.class);
        when(cookie.value()).thenReturn("name");
        processor.processArgument(request, cookie, "one");

        assertTrue(request.getHeaders().containsKey(HttpHeaders.COOKIE), "cookie value resolve fail");
        assertTrue(request.getHeaders().get(HttpHeaders.COOKIE).contains("name=one"), "cookie value resolve error");
    }

    @Test
    public void processArgumentTwoTest() {
        headers.put(HttpHeaders.COOKIE, Lists.newArrayList("one=one"));
        final CookieValue cookie = spy(CookieValue.class);
        when(cookie.value()).thenReturn("two");
        processor.processArgument(request, cookie, "twoValue");

        assertTrue(request.getHeaders().containsKey(HttpHeaders.COOKIE), "cookie value resolve fail");
        assertTrue(request.getHeaders().get(HttpHeaders.COOKIE).contains("two=twoValue"), "cookie value resolve error");
    }

    @Test
    public void processArgumentTemplateTest() {
        RequestTemplate template = new RequestTemplate();
        final Map<String, Collection<String>> headers = Maps.newHashMap();
        headers.put(HttpHeaders.COOKIE, Lists.newArrayList("one=one"));
        template.setHeaders(headers);
        when(request.getRequestTemplate()).thenReturn(template);

        final CookieValue cookie = spy(CookieValue.class);
        when(cookie.value()).thenReturn("two");
        processor.processArgument(request, cookie, "twoValue");

        assertTrue(request.getHeaders().containsKey(HttpHeaders.COOKIE), "cookie value resolve fail");
        assertTrue(request.getHeaders().get(HttpHeaders.COOKIE).contains("two=twoValue"), "cookie value resolve error");
    }

    @AfterEach
    public void clear() {
        headers.clear();
    }

}
