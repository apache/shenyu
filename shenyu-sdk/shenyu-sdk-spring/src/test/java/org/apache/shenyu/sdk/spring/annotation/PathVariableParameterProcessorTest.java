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
import java.util.Map;
import java.util.stream.Stream;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import org.junit.Assert;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import org.springframework.web.bind.annotation.PathVariable;

/**
 * {@link PathVariable} process test.
 */
public class PathVariableParameterProcessorTest {

    private ShenyuRequest request;

    private AnnotatedParameterProcessor processor;

    @BeforeEach
    public void init() {
        this.processor = new PathVariableParameterProcessor();
    }

    @ParameterizedTest
    @MethodSource("org.apache.shenyu.sdk.spring.annotation.PathVariableParameterProcessorTest#templateStream")
    public void processArgumentNullTest(final RequestTemplate template) {
        this.request = ShenyuRequest.create(ShenyuRequest.HttpMethod.POST, "", Maps.newHashMap(), "", "test", template);
        final PathVariable path = spy(PathVariable.class);
        when(path.value()).thenReturn("");

        final Method method = mock(Method.class);
        when(method.getName()).thenReturn("controllerMethod");
        template.setMethod(method);

        Assert.assertThrows(IllegalStateException.class, () -> processor.processArgument(request, path, ""));
    }

    @ParameterizedTest
    @MethodSource("org.apache.shenyu.sdk.spring.annotation.PathVariableParameterProcessorTest#templateStream")
    public void processArgumentStringTest(final RequestTemplate template) {
        this.request = ShenyuRequest.create(ShenyuRequest.HttpMethod.POST, "", Maps.newHashMap(), "", "test", template);
        final PathVariable path = spy(PathVariable.class);
        when(path.value()).thenReturn("id");

        processor.processArgument(request, path, "one");

        assertTrue(request.getUrl().contains("/one"), "path value resolve fail");
    }

    @ParameterizedTest
    @MethodSource("org.apache.shenyu.sdk.spring.annotation.PathVariableParameterProcessorTest#templateStream")
    public void processArgumentMapTest(final RequestTemplate template) {
        this.request = ShenyuRequest.create(ShenyuRequest.HttpMethod.POST, "", Maps.newHashMap(), "", "test", template);
        Map<String, String> pathParam = Maps.newHashMap();
        pathParam.put("id", "idValue");
        pathParam.put("name", "nameValue");
        pathParam.put("type", "typeValue");

        final PathVariable path = spy(PathVariable.class);
        when(path.value()).thenReturn("");
        processor.processArgument(request, path, pathParam);

        assertTrue(request.getUrl().contains("/idValue"), "path value resolve fail");
        assertTrue(request.getUrl().equals("/url/a/b/id/idValue") || request.getUrl().contains("nameValue") || request.getUrl().contains("typeValue"), "path value resolve error");
    }

    /**
     * template argument source.
     * @return Stream&lt;RequestTemplate&gt;
     */
    public static Stream<RequestTemplate> templateStream() {
        RequestTemplate template = new RequestTemplate();
        template.setUrl("/url/a/b");
        template.setPath("/id/{id}");
        final RequestTemplate template1 = RequestTemplate.from(template);
        template1.setPath("/id/{id}/name/{name}");
        final RequestTemplate template2 = RequestTemplate.from(template);
        template1.setPath("/id/{id}/type/{type}");
        return Stream.of(template, template1, template2);
    }

}
