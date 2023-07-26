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
import java.math.BigDecimal;
import java.util.Map;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * {@link RequestParam} process test.
 */
public class RequestParamParameterProcessorTest {

    private ShenyuRequest request;

    private AnnotatedParameterProcessor processor;

    private Method method1;

    private Method method2;

    @BeforeEach
    public void init() throws NoSuchMethodException {
        this.processor = new RequestParamParameterProcessor();
        Class<?> clazz = Controller.class;
        this.method1 = clazz.getMethod("method1", String.class);
        this.method2 = clazz.getMethod("method2", Map.class);
    }

    @Test
    public void processArgumentNullTest() {
        RequestTemplate template = new RequestTemplate(Void.class, method1, "method1", "/dev/url/param", "", "/path", ShenyuRequest.HttpMethod.GET, null, null, null);
        this.request = ShenyuRequest.create(ShenyuRequest.HttpMethod.POST, "", Maps.newHashMap(), "", "test", template);
        final RequestParam param = spy(RequestParam.class);
        when(param.value()).thenReturn("");

        assertThrows(IllegalStateException.class, () -> processor.processArgument(request, param, ""));
    }

    @Test
    public void processArgumentStringTest() {
        RequestTemplate template = new RequestTemplate(Void.class, method1, "method1", "/dev/url/param", "", "/path", ShenyuRequest.HttpMethod.GET, null, null, null);
        this.request = ShenyuRequest.create(ShenyuRequest.HttpMethod.POST, "", Maps.newHashMap(), "", "test", template);
        final RequestParam param = spy(RequestParam.class);
        when(param.value()).thenReturn("id");

        processor.processArgument(request, param, "idValue");

        assertTrue(request.getUrl().endsWith("id=idValue"), "param resolve failed.");
    }

    @Test
    public void processArgumentMapTest() {
        RequestTemplate template = new RequestTemplate(Void.class, method2, "method1", "/dev/url/param", "", "/path", ShenyuRequest.HttpMethod.GET, null, null, null);
        this.request = ShenyuRequest.create(ShenyuRequest.HttpMethod.POST, "", Maps.newHashMap(), "", "test", template);
        final RequestParam param = spy(RequestParam.class);

        Map<String, Object> params = Maps.newHashMap();
        params.put("id", Integer.SIZE);
        params.put("name", "nameValue");
        params.put("price", BigDecimal.ONE);
        processor.processArgument(request, param, params);

        assertTrue(request.getUrl().contains("id=32"), "param resolve failed.");
        assertTrue(request.getUrl().contains("name=nameValue"), "param resolve failed.");
        assertTrue(request.getUrl().contains("price=1"), "param resolve failed.");
    }

    public interface Controller {

        /**
         * method1 for test.
         * @param param param
         */
        void method1(@RequestParam String param);

        /**
         * method2 for test.
         * @param param param
         */
        void method2(@RequestParam Map<String, Object> param);
    }

}
