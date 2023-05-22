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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class SpringMvcPreApiMetaBeanParserTest {

    private SpringMvcPreApiMetaBeanParser springMvcPreApiMetaBeanParser;

    @BeforeEach
    public void init() {

        ClientRegisterConfig clientRegisterConfig = mock(ClientRegisterConfig.class);
        when(clientRegisterConfig.getAddPrefixed()).thenReturn(true);
        when(clientRegisterConfig.getAppName()).thenReturn("http");
        when(clientRegisterConfig.getHost()).thenReturn("127.0.0.1");
        when(clientRegisterConfig.getContextPath()).thenReturn("/http");

        springMvcPreApiMetaBeanParser = new SpringMvcPreApiMetaBeanParser(clientRegisterConfig);
    }

    @Test
    public void testParse() throws Exception {
        ApiBean apiBean = new ApiBean("/http",
                "testClass", TestBeanMatchClass.class.getDeclaredConstructor().newInstance(),
                "/testClass", TestBeanMatchClass.class);

        MetaDataRegisterDTO apiMeta = springMvcPreApiMetaBeanParser
                .parse(apiBean);
        assertThat(apiMeta.getPath(), is("/http/testClass/**"));
    }

    @ShenyuSpringMvcClient("/testClass/**")
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
