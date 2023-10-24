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

import com.google.common.collect.Maps;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import org.junit.jupiter.api.AfterEach;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import org.springframework.http.HttpHeaders;
import org.springframework.web.bind.annotation.RequestHeader;

/**
 * {@link RequestHeader} process test.
 */
public class RequestHeaderParameterProcessorTest {

    private ShenyuRequest request;

    private final Map<String, Collection<String>> headers = new HashMap<>();

    private AnnotatedParameterProcessor processor;

    @BeforeEach
    public void init() {
        this.processor = new RequestHeaderParameterProcessor();
        final ShenyuRequest req = mock(ShenyuRequest.class);
        final RequestTemplate template = mock(RequestTemplate.class);
        final Method method = mock(Method.class);
        when(req.getRequestTemplate()).thenReturn(template);
        when(template.getHeaders()).thenReturn(Maps.newHashMap());
        when(method.getName()).thenReturn("controllerMethod");
        when(template.getMethod()).thenReturn(method);
        when(req.getHeaders()).thenReturn(headers);
        this.request = req;
    }

    @Test
    public void processArgumentEmptyTest() {
        final RequestHeader header = spy(RequestHeader.class);
        when(header.value()).thenReturn("");
        assertThrows(IllegalStateException.class, () -> processor.processArgument(request, header, "value1"));
    }

    @Test
    public void processArgumentStringTest() {
        final String headerName = "header1";
        final String headerValue = "value1";

        final RequestHeader header = spy(RequestHeader.class);
        when(header.value()).thenReturn(headerName);

        processor.processArgument(request, header, "value1");

        assertTrue(headers.get(headerName).contains(headerValue));

    }

    @Test
    public void processArgumentMapTest() {
        final String headerName = "header";
        final RequestHeader header = spy(RequestHeader.class);
        when(header.value()).thenReturn(headerName);

        Map<String, String> map = Maps.newHashMap();
        map.put("header1", "value1");
        map.put("header2", "value2");
        processor.processArgument(request, header, map);

        assertTrue(request.getHeaders().get("header1").contains("value1"));
        assertTrue(request.getHeaders().get("header2").contains("value2"));
    }

    @Test
    public void processArgumentHttpHeaderTest() {
        final String headerName = "header";
        final RequestHeader header = spy(RequestHeader.class);
        when(header.value()).thenReturn(headerName);

        HttpHeaders headers = new HttpHeaders();
        headers.add("header1", "value1");
        headers.add("header2", "value2");
        headers.add("header1", "value2");

        processor.processArgument(request, header, headers);

        assertTrue(request.getHeaders().get("header1").contains("value1"));
        assertTrue(request.getHeaders().get("header1").contains("value2"));
        assertTrue(request.getHeaders().get("header2").contains("value2"));
    }

    @AfterEach
    public void clear() {
        headers.clear();
    }

}
