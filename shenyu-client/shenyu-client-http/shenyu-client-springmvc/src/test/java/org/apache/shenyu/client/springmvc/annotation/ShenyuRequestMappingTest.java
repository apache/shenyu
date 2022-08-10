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
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;


/**
 * Test for {@link ShenyuRequestMapping}.
 */
@ExtendWith(MockitoExtension.class)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class ShenyuRequestMappingTest {

    private final SpringMvcClientTestBean springMvcClientTestBean = new SpringMvcClientTestBean();

    @Test
    public void testWithShenyuRequestMappingAnnotation() {
        Class<?> clazz = springMvcClientTestBean.getClass();
        final ShenyuSpringMvcClient shenyuSpringMvcClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuSpringMvcClient.class);
        assertNotNull(shenyuSpringMvcClient);
        assertEquals(shenyuSpringMvcClient.value(), "/order");
        assertEquals(shenyuSpringMvcClient.path(), "/order");
        assertEquals(shenyuSpringMvcClient.ruleName(), "divide");
        assertFalse(shenyuSpringMvcClient.enabled());
        assertEquals(shenyuSpringMvcClient.desc(), "it`s test.");
        final RequestMapping requestMapping = AnnotatedElementUtils.findMergedAnnotation(clazz, RequestMapping.class);
        assertNotNull(requestMapping);
        assertEquals(requestMapping.value()[0], "/order");
        assertEquals(requestMapping.path()[0], "/order");
    }

    @RestController
    @ShenyuRequestMapping(value = "/order", ruleName = "divide", enabled = false, desc = "it`s test.")
    static class SpringMvcClientTestBean {

        public String hello() {
            return "hello";
        }
    }

}
