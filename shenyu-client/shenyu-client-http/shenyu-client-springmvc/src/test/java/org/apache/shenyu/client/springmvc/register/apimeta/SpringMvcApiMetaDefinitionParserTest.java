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
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.lang.reflect.Method;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class SpringMvcApiMetaDefinitionParserTest {

    private SpringMvcApiMetaDefinitionParser apiMetaDefinitionParser;

    @BeforeEach
    public void init() {

        ClientRegisterConfig clientRegisterConfig = mock(ClientRegisterConfig.class);
        when(clientRegisterConfig.getAddPrefixed()).thenReturn(true);
        when(clientRegisterConfig.getAppName()).thenReturn("http");
        when(clientRegisterConfig.getHost()).thenReturn("127.0.0.1");
        when(clientRegisterConfig.getContextPath()).thenReturn("/http");

        apiMetaDefinitionParser = new SpringMvcApiMetaDefinitionParser(clientRegisterConfig);
    }

    @Test
    public void testParse() throws Exception {
        Method method = TestBeanMatchClass.class.getMethod("testMethod");

        ApiBean.ApiDefinition apiDefinition =
                createApiDefinition(TestBeanMatchClass.class, method, "/testMethod");

        List<MetaDataRegisterDTO> apiMetas = apiMetaDefinitionParser.parse(apiDefinition);

        assertThat(apiMetas.size(), is(1));
        MetaDataRegisterDTO apiMeta = apiMetas.get(0);
        assertThat(apiMeta.getPath(), is("/http/testClass/testMethod"));
    }

    @Test
    public void testParseWithPathVariable() throws Exception {
        Method method = TestBeanMatchClass.class.getMethod("testMethod", Integer.class);
        ApiBean.ApiDefinition apiDefinition =
                createApiDefinition(TestBeanMatchClass.class, method, "/testMethod/{id}/info");

        List<MetaDataRegisterDTO> apiMetas = apiMetaDefinitionParser.parse(apiDefinition);

        assertThat(apiMetas.size(), is(1));
        MetaDataRegisterDTO apiMeta = apiMetas.get(0);
        assertThat(apiMeta.getPath(), is("/http/testClass/testMethod/**/info"));
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
    static class TestBeanMatchClass {

        @RequestMapping("/testMethod")
        public String testMethod() {
            return "";
        }

        @RequestMapping("/testMethod/{id}/info")
        @ShenyuSpringMvcClient("/testMethod/**/info")
        public String testMethod(@PathVariable final Integer id) {
            return "";
        }
    }
}
