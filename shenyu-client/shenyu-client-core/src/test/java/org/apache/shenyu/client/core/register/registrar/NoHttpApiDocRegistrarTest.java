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

import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.apidocs.annotations.ApiModule;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class NoHttpApiDocRegistrarTest {
    private final NoHttpApiDocRegistrar noHttpApiDocRegistrar =
            new NoHttpApiDocRegistrar(null, new TestClientRegisterConfig());
    
    @Test
    public void testDoParse() {
        final TestApiBeanAnnotatedClassAndMethod bean = new TestApiBeanAnnotatedClassAndMethod();
        
        ApiBean apiBean = new ApiBean(RpcTypeEnum.HTTP.getName(), "bean", bean);
        
        apiBean.addApiDefinition(null, null);
        
        AbstractApiDocRegistrar.HttpApiSpecificInfo httpApiSpecificInfo =
                noHttpApiDocRegistrar.doParse(apiBean.getApiDefinitions().get(0));
        
        assertThat(httpApiSpecificInfo.getApiHttpMethodEnums().get(0), is(ApiHttpMethodEnum.NOT_HTTP));
        
        assertThat(httpApiSpecificInfo.getConsume(), is(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE));
        
        assertThat(httpApiSpecificInfo.getProduce(), is(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE));
    }
    
    @ApiModule("testClass")
    static class TestApiBeanAnnotatedClassAndMethod {
        @ApiDoc(desc = "testMethod")
        public String testMethod() {
            return "";
        }
    }
}
