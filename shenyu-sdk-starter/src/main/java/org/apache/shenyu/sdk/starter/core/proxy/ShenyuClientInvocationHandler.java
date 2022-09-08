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

package org.apache.shenyu.sdk.starter.core.proxy;

import org.apache.shenyu.sdk.starter.core.RequestTemplate;
import org.apache.shenyu.sdk.starter.core.ShenyuClient;
import org.apache.shenyu.sdk.starter.core.ShenyuClientFactoryBean;
import org.apache.shenyu.sdk.starter.core.ShenyuHttpClient;
import org.apache.shenyu.sdk.starter.core.factory.Contract;
import org.apache.shenyu.sdk.starter.core.factory.RequestPostProcessor;
import org.springframework.context.ApplicationContext;
import org.springframework.util.ObjectUtils;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ShenyuClientInvocationHandler implements InvocationHandler {

    private final Map<Method, ShenyuClientMethodHandler> methodHandlerMap = new ConcurrentHashMap<>();

    private final ShenyuHttpClient shenyuHttpClient;

    private final Contract contract;

    private final ShenyuClientFactoryBean shenyuClientFactoryBean;

    private final Collection<RequestPostProcessor> requestPostProcessors;

    public ShenyuClientInvocationHandler(final Class<?> apiClass, final ApplicationContext applicationContext,
                                         final ShenyuClientFactoryBean shenyuClientFactoryBean) {
        this.shenyuClientFactoryBean = shenyuClientFactoryBean;
        this.shenyuHttpClient = applicationContext.getBean(ShenyuHttpClient.class);
        this.contract = applicationContext.getBean(Contract.class);
        final Map<String, RequestPostProcessor> beansOfType = applicationContext.getBeansOfType(RequestPostProcessor.class);
        this.requestPostProcessors = beansOfType.values();
        ShenyuClient shenyuClient = apiClass.getAnnotation(ShenyuClient.class);
        buildMethodHandlerMap(apiClass, shenyuHttpClient, shenyuClient);
    }

    @Override
    public Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
        ShenyuClientMethodHandler handler = methodHandlerMap.get(method);
        if (ObjectUtils.isEmpty(handler)) {
            return handler.invoke(args);
        }
        return null;
    }

    private void buildMethodHandlerMap(final Class<?> apiClass, final ShenyuHttpClient shenyuHttpClient, final ShenyuClient shenyuClient) {
        final List<RequestTemplate> requestTemplates = contract.parseAndValidateMetadata(apiClass);
        for (RequestTemplate requestTemplate : requestTemplates) {
            requestTemplate.setUrl(shenyuClientFactoryBean.getUrl());
            methodHandlerMap.put(requestTemplate.getMethod(),
                    new ShenyuClientMethodHandler(shenyuClient, shenyuHttpClient, requestTemplate, requestPostProcessors));
        }
    }
}
