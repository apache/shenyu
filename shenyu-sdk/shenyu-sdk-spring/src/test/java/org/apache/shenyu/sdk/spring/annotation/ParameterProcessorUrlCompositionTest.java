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

import java.lang.reflect.Method;
import java.util.HashMap;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * {@link PathVariable} and {@link RequestParam} url composition test.
 */
public final class ParameterProcessorUrlCompositionTest {

    private static final String BASE_URL = "http://localhost:9195";

    private PathVariableParameterProcessor pathVariableProcessor;

    private RequestParamParameterProcessor requestParamProcessor;

    private Method clientMethod;

    @BeforeEach
    public void init() throws NoSuchMethodException {
        this.pathVariableProcessor = new PathVariableParameterProcessor();
        this.requestParamProcessor = new RequestParamParameterProcessor();
        this.clientMethod = TestClient.class.getMethod("get", String.class, String.class);
    }

    @Test
    public void pathVariableThenRequestParamCompose() {
        final ShenyuRequest request = newRequest("/orders/{id}");

        this.pathVariableProcessor.processArgument(request, annotation(PathVariable.class), "one");
        this.requestParamProcessor.processArgument(request, annotation(RequestParam.class), "all");

        assertEquals(BASE_URL + "/orders/one?expand=all", request.getUrl());
    }

    @Test
    public void requestParamThenPathVariableCompose() {
        final ShenyuRequest request = newRequest("/orders/{id}");

        this.requestParamProcessor.processArgument(request, annotation(RequestParam.class), "all");
        this.pathVariableProcessor.processArgument(request, annotation(PathVariable.class), "one");

        assertEquals(BASE_URL + "/orders/one?expand=all", request.getUrl());
    }

    private <T extends java.lang.annotation.Annotation> T annotation(final Class<T> annotationType) {
        return this.clientMethod.getParameters()[0].isAnnotationPresent(annotationType)
                ? this.clientMethod.getParameters()[0].getAnnotation(annotationType)
                : this.clientMethod.getParameters()[1].getAnnotation(annotationType);
    }

    private ShenyuRequest newRequest(final String path) {
        final RequestTemplate template = new RequestTemplate(Void.class, this.clientMethod, "get",
                BASE_URL, "", path, ShenyuRequest.HttpMethod.GET, null, null, null);
        return ShenyuRequest.create(ShenyuRequest.HttpMethod.GET,
                template.getUrl() + template.getPath(), new HashMap<>(), "", "test", template);
    }

    interface TestClient {

        @GetMapping("/orders/{id}")
        Object get(@PathVariable("id") String id, @RequestParam("expand") String expand);
    }
}
