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

package org.apache.shenyu.client.core.register.registrar;

import com.google.common.collect.Lists;
import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.apidocs.annotations.ApiModule;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;

public class AbstractApiDocRegistrarTest {
    
    private TestShenyuClientRegisterEventPublisher testShenyuClientRegisterEventPublisher;
    
    private AbstractApiDocRegistrar apiDocRegistrar;
    
    @BeforeEach
    public void init() {
        
        testShenyuClientRegisterEventPublisher = new TestShenyuClientRegisterEventPublisher();
        
        apiDocRegistrar = new SimpleApiDocRegistrar(
                testShenyuClientRegisterEventPublisher, new TestClientRegisterConfig());
    }
    
    @Test
    void testAnnotatedClass() throws Exception {
        
        ApiBean apiBean = createSimpleApiBean(TestApiBeanAnnotatedClass.class);
        
        apiDocRegistrar.register(apiBean);
        
        assertThat(testShenyuClientRegisterEventPublisher.metaData,
                nullValue());
    }
    
    @Test
    void testAnnotatedMethod() throws Exception {
        
        ApiBean apiBean = createSimpleApiBean(TestApiBeanAnnotatedMethod.class);
        
        apiDocRegistrar.register(apiBean);
        
        assertThat(testShenyuClientRegisterEventPublisher.metaData,
                nullValue());
    }
    
    @Test
    void testAnnotatedMethodAndClass() throws Exception {
        
        ApiBean apiBean = createSimpleApiBean(TestApiBeanAnnotatedClassAndMethod.class);
        
        apiDocRegistrar.register(apiBean);
        
        assertThat(testShenyuClientRegisterEventPublisher.metaData.getApiPath(),
                equalTo("/testContext/testClass/testMethod"));
    }
    
    private static ApiBean createSimpleApiBean(final Class<?> beanClass) throws Exception {
        
        ApiBean apiBean = new ApiBean(RpcTypeEnum.HTTP.getName(),
                beanClass.getName(), beanClass.getDeclaredConstructor().newInstance(), "testClass");
        
        apiBean.addApiDefinition(beanClass.getMethod("testMethod"), "/testMethod");
        
        return apiBean;
    }
    
    @ApiModule("testClass")
    @RestController
    @RequestMapping("/testClass")
    static class TestApiBeanAnnotatedClassAndMethod {
        @RequestMapping("/testMethod")
        @ApiDoc(desc = "testMethod")
        public String testMethod() {
            return "";
        }
    }
    
    @RestController
    @RequestMapping("/testClass")
    static class TestApiBeanAnnotatedMethod {
        
        @RequestMapping("/testMethod")
        @ApiDoc(desc = "testMethod")
        public String testMethod() {
            return "";
        }
    }
    
    @RestController
    @ApiModule("testMethod")
    @RequestMapping("/testClass")
    static class TestApiBeanAnnotatedClass {
        
        @RequestMapping("/testMethod")
        public String testMethod() {
            return "";
        }
    }
    
    public static class SimpleApiDocRegistrar extends AbstractApiDocRegistrar {
        
        protected SimpleApiDocRegistrar(final ShenyuClientRegisterEventPublisher publisher, final ClientRegisterConfig clientRegisterConfig) {
            super(publisher, clientRegisterConfig);
        }
        
        @Override
        protected HttpApiSpecificInfo doParse(final ApiBean.ApiDefinition apiDefinition) {
            
            String produce = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
            
            String consume = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
            
            List<ApiHttpMethodEnum> apiHttpMethodEnums = Lists.newArrayList(ApiHttpMethodEnum.NOT_HTTP);
            
            return new HttpApiSpecificInfo(produce, consume, apiHttpMethodEnums);
        }
    }
    
    static class TestShenyuClientRegisterEventPublisher extends ShenyuClientRegisterEventPublisher {
        
        private ApiDocRegisterDTO metaData;
        
        @Override
        public void start(final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        }
        
        @Override
        public void publishEvent(final DataTypeParent data) {
            this.metaData = (ApiDocRegisterDTO) data;
        }
    }
}
