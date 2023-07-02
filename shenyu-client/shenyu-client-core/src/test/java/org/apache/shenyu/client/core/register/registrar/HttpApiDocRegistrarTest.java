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
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;

public class HttpApiDocRegistrarTest {
    
    private final HttpApiDocRegistrar httpApiDocRegistrar =
            new HttpApiDocRegistrar(null, new TestClientRegisterConfig());
    
    @Test
    public void testDoParse() throws Exception {
        
        ApiBean apiBean = createSimpleApiBean();
        
        AbstractApiDocRegistrar.HttpApiSpecificInfo httpApiSpecificInfo =
                httpApiDocRegistrar.doParse(apiBean.getApiDefinitions().get(0));
        List<ApiHttpMethodEnum> apiHttpMethodEnums =
                Lists.newArrayList(ApiHttpMethodEnum.values());
        
        apiHttpMethodEnums.remove(ApiHttpMethodEnum.NOT_HTTP);
        
        assertThat(httpApiSpecificInfo.getApiHttpMethodEnums(), Matchers.contains(
                apiHttpMethodEnums.toArray(new ApiHttpMethodEnum[0])));
        
        assertThat(httpApiSpecificInfo.getConsume(), Matchers.is("application/json"));
        
        assertThat(httpApiSpecificInfo.getProduce(), Matchers.is("application/json"));
        
    }
    
    private ApiBean createSimpleApiBean() throws Exception {
        
        ApiBean apiBean = new ApiBean(RpcTypeEnum.HTTP.getName(),
                TestApiBeanAnnotatedClassAndMethod.class.getName(), ((Class<?>) TestApiBeanAnnotatedClassAndMethod.class).getDeclaredConstructor().newInstance());
        
        apiBean.addApiDefinition(TestApiBeanAnnotatedClassAndMethod.class.getMethod("testMethod"), "/testMethod");
        
        return apiBean;
    }
    
    @ApiModule("testClass")
    @RestController
    @RequestMapping("/testClass")
    static class TestApiBeanAnnotatedClassAndMethod {
        @RequestMapping(value = "/testMethod", consumes = "application/json", produces = "application/json")
        @ApiDoc(desc = "testMethod")
        public String testMethod() {
            return "";
        }
    }
    
}
