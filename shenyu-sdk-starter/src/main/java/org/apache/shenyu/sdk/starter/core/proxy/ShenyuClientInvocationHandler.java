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
import org.apache.shenyu.sdk.starter.core.factory.RequestPostProcessor;
import org.apache.shenyu.sdk.starter.core.support.SpringMvcContract;
import org.apache.shenyu.sdk.starter.core.util.Util;
import org.springframework.context.ApplicationContext;
import org.springframework.util.ObjectUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ShenyuClientInvocationHandler implements InvocationHandler {

    private final Map<Method, ShenyuClientMethodHandler> methodHandlerMap = new ConcurrentHashMap<>();

    private final ApplicationContext applicationContext;

    private final ShenyuHttpClient shenyuHttpClient;

    private final SpringMvcContract springMvcContract;

    private final ShenyuClientFactoryBean shenyuClientFactoryBean;

    private final Collection<RequestPostProcessor> requestPostProcessors;

    public ShenyuClientInvocationHandler(final Class<?> apiClass, final ApplicationContext applicationContext, final ShenyuClientFactoryBean shenyuClientFactoryBean) {
        this.applicationContext = applicationContext;
        this.shenyuClientFactoryBean = shenyuClientFactoryBean;
        ShenyuClient shenyuClient = apiClass.getAnnotation(ShenyuClient.class);
        if (shenyuClient == null) {
            throw new NullPointerException("NOT FOUND @ShenyuClient BY " + apiClass.getName());
        }
        this.shenyuHttpClient = applicationContext.getBean(ShenyuHttpClient.class);
        this.springMvcContract = applicationContext.getBean(SpringMvcContract.class);
        final Map<String, RequestPostProcessor> beansOfType = applicationContext.getBeansOfType(RequestPostProcessor.class);
        this.requestPostProcessors = beansOfType.values();
        buildMethodHandlerMap(apiClass, shenyuHttpClient, shenyuClient);
    }

    private void buildMethodHandlerMap(final Class<?> apiClass, final ShenyuHttpClient shenyuHttpClient, final ShenyuClient shenyuClient) {
        Method[] methods = apiClass.getMethods();

        for (Method method : methods) {
            if (method.getDeclaringClass() == Object.class
                    || (method.getModifiers() & Modifier.STATIC) != 0
                    || Util.isDefault(method)) {
                continue;
            }
            final RequestTemplate requestTemplate = springMvcContract.parseRequestTemplate(method);
            requestTemplate.setUrl(shenyuClientFactoryBean.getUrl());
            requestTemplate.setParamMetadataList(this.analysisParamMetadata(method));
            methodHandlerMap.put(method, new ShenyuClientMethodHandler(shenyuClient, shenyuHttpClient, method, requestTemplate, requestPostProcessors));
        }
    }

    @Override
    public Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
        ShenyuClientMethodHandler handler = methodHandlerMap.get(method);
        if (ObjectUtils.isEmpty(handler)) {
            return handler.invoke(args);
        }
        return null;
    }

    /**
     * analysisParamMetadata.
     *
     * @param method method
     * @return {@link List}
     */
    private List<RequestTemplate.ParamMetadata> analysisParamMetadata(final Method method) {
        List<RequestTemplate.ParamMetadata> params = new ArrayList<>();
        Parameter[] parameters = method.getParameters();
        if (parameters != null && parameters.length > 0) {
            for (int index = 0; index < parameters.length; index++) {
                Annotation[] annotations = parameters[index].getAnnotations();
                if (annotations == null || annotations.length == 0) {
                    continue;
                }
                RequestTemplate.ParamMetadata paramMetadata = new RequestTemplate.ParamMetadata();
                paramMetadata.setParamAnnotations(annotations);
                paramMetadata.setParamIndexOnMethod(index);
                paramMetadata.setParamType(parameters[index].getType());
                paramMetadata.setPrimitive(parameters[index].getType().isPrimitive());
                params.add(paramMetadata);
            }
        }
        return params;
    }

}
