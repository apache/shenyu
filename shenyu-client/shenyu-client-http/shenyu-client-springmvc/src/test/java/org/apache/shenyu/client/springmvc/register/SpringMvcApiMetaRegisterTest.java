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

package org.apache.shenyu.client.springmvc.register;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;

public class SpringMvcApiMetaRegisterTest {
    
    private TestShenyuClientRegisterEventPublisher testShenyuClientRegisterEventPublisher;
    
    private SpringMvcApiMetaRegister springMvcApiMetaRegister;
    
    private final SpringMvcApiBeansExtractor beansExtractor = SpringMvcApiBeansExtractor.buildDefaultSpringMvcApiBeansExtractor();
    
    @BeforeEach
    public void init() {
        
        testShenyuClientRegisterEventPublisher = new TestShenyuClientRegisterEventPublisher();
        
        springMvcApiMetaRegister = new SpringMvcApiMetaRegister(
                testShenyuClientRegisterEventPublisher, new TestClientRegisterConfig());
        
        beansExtractor.addSupportedApiAnnotations(ShenyuSpringMvcClient.class);
        beansExtractor.addSupportedApiDefinitionAnnotations(ShenyuSpringMvcClient.class);
    }
    
    @Test
    void testAnnotatedClass() throws Exception {
        
        ApiBean apiBean = createSimpleApiBean(TestApiBeanAnnotatedClass.class);
        
        springMvcApiMetaRegister.register(apiBean);
        
        assertThat(testShenyuClientRegisterEventPublisher.metaData.getPath(),
                equalTo("/testContext/testClass/testMethod"));
    }
    
    @Test
    void testAnnotatedMethod() throws Exception {
        
        ApiBean apiBean = createSimpleApiBean(TestApiBeanAnnotatedMethod.class);
        
        springMvcApiMetaRegister.register(apiBean);
        
        assertThat(testShenyuClientRegisterEventPublisher.metaData.getPath(),
                equalTo("/testContext/testClass/testMethod"));
    }
    
    @Test
    void testAnnotatedMethodAndClass() throws Exception {
        
        ApiBean apiBean = createSimpleApiBean(TestApiBeanAnnotatedMethodAndClass.class);
        
        springMvcApiMetaRegister.register(apiBean);
        
        assertThat(testShenyuClientRegisterEventPublisher.metaData.getPath(),
                equalTo("/testContext/testClass/testMethod"));
    }
    
    @Test
    void testApiBeanNoAnnotated() throws Exception {
        
        ApiBean apiBean = createSimpleApiBean(TestApiBeanNoAnnotated.class);
        
        springMvcApiMetaRegister.register(apiBean);
        
        assertThat(testShenyuClientRegisterEventPublisher.metaData, nullValue());
    }
    
    @Test
    void testPreApiBean() throws Exception {
        
        ApiBean apiBean = createSimpleApiBean(TestPreApiBean.class);
        
        springMvcApiMetaRegister.register(apiBean);
        
        assertThat(testShenyuClientRegisterEventPublisher.metaData.getPath(),
                equalTo("/testContext/testClass/**"));
        
    }
    
    @Test
    void testApiBeanDifferentPath() throws Exception {
        
        ApiBean apiBean = createSimpleApiBean(TestApiBeanDifferentPath.class);
        
        springMvcApiMetaRegister.register(apiBean);
        
        assertThat(testShenyuClientRegisterEventPublisher.metaData.getPath(),
                equalTo("/testContext/testClassPath/testMethodPath"));
        
    }
    
    private ApiBean createSimpleApiBean(final Class<?> beanClass) throws Exception {
        final Map<String, Object> objectHashMap = new HashMap<>();
        objectHashMap.put(beanClass.getName(), beanClass.getDeclaredConstructor().newInstance());
        return beansExtractor.extract(null, objectHashMap).get(0);
    }
    
    @ShenyuSpringMvcClient
    @RestController
    @RequestMapping("/testClass")
    static class TestApiBeanAnnotatedClass {
        @RequestMapping("/testMethod")
        public String testMethod() {
            return "";
        }
    }
    
    @RestController
    @RequestMapping("/testClass")
    static class TestApiBeanAnnotatedMethod {
        
        @RequestMapping("/testMethod")
        @ShenyuSpringMvcClient("/testMethod")
        public String testMethod() {
            return "";
        }
    }
    
    @ShenyuSpringMvcClient
    @RestController
    @RequestMapping("/testClass")
    static class TestApiBeanAnnotatedMethodAndClass {
        
        @RequestMapping("/testMethod")
        @ShenyuSpringMvcClient("/testMethod")
        public String testMethod() {
            return "";
        }
    }
    
    @RestController
    @ShenyuSpringMvcClient("/testClass/**")
    @RequestMapping("/testClass")
    static class TestPreApiBean {
        
        @RequestMapping("/testMethod")
        @ShenyuSpringMvcClient
        public String testMethod() {
            return "";
        }
    }
    
    @RestController
    @RequestMapping("/testClass")
    static class TestApiBeanNoAnnotated {
        
        @RequestMapping("/testMethod")
        public String testMethod() {
            return "";
        }
    }
    
    @RestController
    @ShenyuSpringMvcClient("/testClassPath")
    @RequestMapping("/testClass")
    static class TestApiBeanDifferentPath {
        
        @RequestMapping("/testMethod")
        @ShenyuSpringMvcClient("/testMethodPath")
        public String testMethod() {
            return "";
        }
    }
    
    static class TestShenyuClientRegisterEventPublisher extends ShenyuClientRegisterEventPublisher {
        
        private MetaDataRegisterDTO metaData;
        
        @Override
        public void start(final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        }
        
        @Override
        public void publishEvent(final DataTypeParent data) {
            this.metaData = (MetaDataRegisterDTO) data;
        }
    }
    
    static class TestClientRegisterConfig implements ClientRegisterConfig {
        @Override
        public Integer getPort() {
            return -1;
        }
        
        @Override
        public String getHost() {
            return "127.0.0.1";
        }
        
        @Override
        public String getAppName() {
            return "test";
        }
        
        @Override
        public String getContextPath() {
            return "testContext";
        }
        
        @Override
        public String getIpAndPort() {
            return "127.0.0.1:80";
        }
        
        @Override
        public Boolean getAddPrefixed() {
            return false;
        }
        
        @Override
        public RpcTypeEnum getRpcTypeEnum() {
            return RpcTypeEnum.HTTP;
        }
    }
    
}
