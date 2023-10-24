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

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class HttpApiDocRegistrar extends AbstractApiDocRegistrar {

    public HttpApiDocRegistrar(final ShenyuClientRegisterEventPublisher publisher,
                               final ClientRegisterConfig clientRegisterConfig) {
        super(publisher, clientRegisterConfig);
    }

    @Override
    protected HttpApiSpecificInfo doParse(final ApiBean.ApiDefinition apiDefinition) {

        RequestMapping requestMapping = apiDefinition.getAnnotation(RequestMapping.class);

        String produce = requestMapping.produces().length == 0 ? ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE : String.join(",", requestMapping.produces());
        String consume = requestMapping.consumes().length == 0 ? ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE : String.join(",", requestMapping.consumes());

        RequestMethod[] requestMethods = requestMapping.method();
        if (requestMethods.length == 0) {
            requestMethods = RequestMethod.values();
        }

        List<ApiHttpMethodEnum> apiHttpMethodEnums =
                Stream.of(requestMethods)
                        .map(item -> ApiHttpMethodEnum.of(item.name()))
                        .collect(Collectors.toList());

        return new HttpApiSpecificInfo(produce, consume, apiHttpMethodEnums);
    }
}
