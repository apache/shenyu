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

package org.apache.shenyu.client.springcloud.annotation;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestBody;

import java.lang.reflect.Method;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Test for {@link ShenyuPutMapping}.
 */
@ExtendWith(MockitoExtension.class)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class ShenyuPutMappingTest {

    private final SpringCloudClientTestBean springCloudClientTestBean = new SpringCloudClientTestBean();

    @Test
    public void testWithShenyuRequestMappingAnnotation() {
        Class<?> clazz = springCloudClientTestBean.getClass();
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            if (!Objects.equals(method.getName(), "put")) {
                continue;
            }
            final RequestMapping requestMapping = AnnotatedElementUtils.findMergedAnnotation(method, RequestMapping.class);
            assertNotNull(requestMapping);
            assertEquals(requestMapping.method()[0], RequestMethod.PUT);
            assertEquals(requestMapping.value()[0], "/put");
            assertEquals(requestMapping.path()[0], "/put");
            ShenyuSpringCloudClient shenyuSpringCloudClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringCloudClient.class);
            assertNotNull(shenyuSpringCloudClient);
            assertEquals(shenyuSpringCloudClient.value(), "/put");
            assertEquals(shenyuSpringCloudClient.path(), "/put");
            assertEquals(shenyuSpringCloudClient.ruleName(), "divide");
            assertFalse(shenyuSpringCloudClient.enabled());
            assertEquals(shenyuSpringCloudClient.desc(), "it`s test.");
        }
    }

    static class SpringCloudClientTestBean {

        @ShenyuPutMapping(value = "/put", ruleName = "divide", enabled = false, desc = "it`s test.")
        public String put(@RequestBody final String body) {
            return "" + body;
        }
    }
}
