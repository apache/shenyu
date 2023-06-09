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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class SpringMvcApiMetaBeanMatcherTest {

    private final SpringMvcApiMetaBeanMatcher beanMatcher = new SpringMvcApiMetaBeanMatcher();

    @Test
    public void testBeanMatch() throws Exception {
        ApiBean apiBean = createSimpleApiBean(TestBeanMatchClass.class);
        boolean result = beanMatcher.match(apiBean);
        assertThat(result, is(true));
    }

    @Test
    public void testBeanMatchWithoutAnnotation() throws Exception {

        ApiBean apiBean = createSimpleApiBean(TestBeanMatchWithoutAnnotationClass.class);
        boolean result = beanMatcher.match(apiBean);
        assertThat(result, is(true));
    }

    @Test
    public void testBeanMatchWithStarAnnotation() throws Exception {

        ApiBean apiBean = createSimpleApiBean(TestBeanMatchWithStarAnnotationClass.class);
        boolean result = beanMatcher.match(apiBean);
        assertThat(result, is(false));
    }

    private static ApiBean createSimpleApiBean(final Class<?> beanClass) throws Exception {
        ApiBean apiBean = new ApiBean("",
                "", beanClass.getDeclaredConstructor().newInstance(), "", beanClass);
        return apiBean;
    }

    @ShenyuSpringMvcClient
    static class TestBeanMatchClass {
    }

    static class TestBeanMatchWithoutAnnotationClass {
    }

    @ShenyuSpringMvcClient("/**")
    static class TestBeanMatchWithStarAnnotationClass {
    }
}
