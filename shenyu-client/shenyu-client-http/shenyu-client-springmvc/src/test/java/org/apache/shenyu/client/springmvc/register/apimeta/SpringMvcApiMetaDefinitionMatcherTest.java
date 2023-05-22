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

package org.apache.shenyu.client.springmvc.register.apimeta;

import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.junit.jupiter.api.Test;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.lang.reflect.Method;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class SpringMvcApiMetaDefinitionMatcherTest {

    private final SpringMvcApiMetaDefinitionMatcher apiDefinitionMetaMatcher =
            new SpringMvcApiMetaDefinitionMatcher();

    @Test
    public void testMatchAnnotatedClass() throws Exception {
        Method method = TestBeanMatchAnnotatedClass.class.getMethod("testMethod");
        ApiBean.ApiDefinition apiDefinition =
                createApiDefinition(TestBeanMatchAnnotatedClass.class, method, "/testMethod");
        boolean result = apiDefinitionMetaMatcher.match(apiDefinition);

        assertThat(result, is(true));
    }

    @Test
    public void testMatchAnnotatedMethod() throws Exception {
        Method method = TestBeanMatchClass.class.getMethod("testAnnotatedMethod");
        ApiBean.ApiDefinition apiDefinition =
                createApiDefinition(TestBeanMatchClass.class, method, "/testAnnotatedMethod");
        boolean result = apiDefinitionMetaMatcher.match(apiDefinition);

        assertThat(result, is(true));
    }

    @Test
    public void tesMatchWithoutAnnotation() throws Exception {
        Method method = TestBeanMatchClass.class.getMethod("testMethod");
        ApiBean.ApiDefinition apiDefinition =
                createApiDefinition(TestBeanMatchClass.class, method, "/testMethod");
        boolean result = apiDefinitionMetaMatcher.match(apiDefinition);

        assertThat(result, is(false));
    }

    private ApiBean.ApiDefinition createApiDefinition(final Class<?> beanClass, final Method method,
                                                      final String methodPath) throws Exception {
        ApiBean apiBean = new ApiBean("/http",
                "testBeanMatchClass", beanClass.getDeclaredConstructor().newInstance(),
                "/testClass", beanClass);

        apiBean.addApiDefinition(method, methodPath);
        return apiBean.getApiDefinitions().get(0);
    }

    @ShenyuSpringMvcClient
    @RestController
    @RequestMapping("/testClass")
    static class TestBeanMatchAnnotatedClass {
        @RequestMapping("/testMethod")
        public String testMethod() {
            return "";
        }
    }

    @RestController
    @RequestMapping("/testClass")
    static class TestBeanMatchClass {

        @RequestMapping("/testMethod")
        public String testMethod() {
            return "";
        }

        @RequestMapping("/testAnnotatedMethod")
        @ShenyuSpringMvcClient("/testAnnotatedMethodo")
        public String testAnnotatedMethod() {
            return "";
        }
    }
}
