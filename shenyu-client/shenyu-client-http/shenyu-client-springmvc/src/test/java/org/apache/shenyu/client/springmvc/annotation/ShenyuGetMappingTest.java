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

package org.apache.shenyu.client.springmvc.annotation;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.lang.reflect.Method;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;


/**
 * Test for {@link ShenyuGetMapping}.
 */
@ExtendWith(MockitoExtension.class)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class ShenyuGetMappingTest {

    private final SpringMvcClientTestBean springMvcClientTestBean = new SpringMvcClientTestBean();

    @Test
    public void testWithShenyuRequestMappingAnnotation() {
        Class<?> clazz = springMvcClientTestBean.getClass();
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            if (!Objects.equals(method.getName(), "get")) {
                continue;
            }
            final RequestMapping requestMapping = AnnotatedElementUtils.findMergedAnnotation(method, RequestMapping.class);
            assertNotNull(requestMapping);
            assertEquals(requestMapping.method()[0], RequestMethod.GET);
            assertEquals(requestMapping.value()[0], "/{id}");
            assertEquals(requestMapping.path()[0], "/{id}");
            ShenyuSpringMvcClient shenyuSpringMvcClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringMvcClient.class);
            assertNotNull(shenyuSpringMvcClient);
            assertEquals(shenyuSpringMvcClient.value(), "/{id}");
            assertEquals(shenyuSpringMvcClient.path(), "/{id}");
            assertEquals(shenyuSpringMvcClient.ruleName(), "divide");
            assertFalse(shenyuSpringMvcClient.enabled());
            assertEquals(shenyuSpringMvcClient.desc(), "it`s test.");
        }
    }

    static class SpringMvcClientTestBean {

        @ShenyuGetMapping(value = "/{id}", ruleName = "divide", enabled = false, desc = "it`s test.")
        public String get(@PathVariable final String id) {
            return "" + id;
        }
    }
}
