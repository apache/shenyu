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

package org.apache.shenyu.sdk.spring.proxy;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.spring.FallbackFactory;
import org.apache.shenyu.sdk.spring.NoFallbackAvailableException;
import org.apache.shenyu.sdk.spring.ShenyuClient;
import org.apache.shenyu.sdk.spring.ShenyuClientFactoryBean;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import org.apache.shenyu.sdk.spring.factory.Contract;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * ShenyuClientInvocationHandler.
 */
public class ShenyuClientInvocationHandler implements InvocationHandler {
    protected static final Logger LOG = LoggerFactory.getLogger(ShenyuClientInvocationHandler.class);

    private final Map<Method, ShenyuClientMethodHandler> methodHandlerMap = new ConcurrentHashMap<>();

    private final ApplicationContext applicationContext;

    private final Contract contract;

    private final ShenyuClientFactoryBean shenyuClientFactoryBean;

    private final FallbackFactory<?> fallbackFactory;

    public ShenyuClientInvocationHandler(final Class<?> apiClass, final ApplicationContext applicationContext,
                                         final ShenyuClientFactoryBean shenyuClientFactoryBean) {
        this.shenyuClientFactoryBean = shenyuClientFactoryBean;
        this.applicationContext = applicationContext;
        this.contract = applicationContext.getBean(Contract.class);
        ShenyuClient shenyuClient = apiClass.getAnnotation(ShenyuClient.class);
        fallbackFactory = getFallbackFactory(shenyuClientFactoryBean.getFallback(),
                shenyuClientFactoryBean.getFallbackFactory());
        buildMethodHandlerMap(apiClass, shenyuClient);
    }

    @Override
    public Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {

        ShenyuClientMethodHandler handler = methodHandlerMap.get(method);
        if (ObjectUtils.isEmpty(handler)) {
            throw new ShenyuException(String.format("the method cannot be called, please check the annotation and configuration, method %s", method.getName()));
        }
        Object result;
        try {
            result = handler.invoke(args);
        } catch (Throwable throwable) {
            LOG.error("ShenYu Client invoke error  ", throwable);
            if (fallbackFactory == null) {
                throw new NoFallbackAvailableException("No fallback available.", throwable);
            }
            Object fallback = fallbackFactory.create(throwable);
            result = method.invoke(fallback, args);
        }
        return result;
    }

    private void buildMethodHandlerMap(final Class<?> apiClass, final ShenyuClient shenyuClient) {
        // parseAndValidate RequestTemplate
        final List<RequestTemplate> requestTemplates = contract.parseAndValidateRequestTemplate(apiClass, shenyuClientFactoryBean);

        final ShenyuSdkClient shenyuSdkClient = applicationContext.getBean(ShenyuSdkClient.class);
        final Map<String, AnnotatedParameterProcessor> annotatedParameterProcessorMap = applicationContext.getBeansOfType(AnnotatedParameterProcessor.class);
        Collection<AnnotatedParameterProcessor> annotatedParameterProcessors = annotatedParameterProcessorMap.values();
        annotatedParameterProcessors = annotatedParameterProcessors.stream().sorted(Comparator.comparing(AnnotatedParameterProcessor::order)).collect(Collectors.toList());
        Map<Class<? extends Annotation>, AnnotatedParameterProcessor> annotatedArgumentProcessors = toAnnotatedArgumentProcessorMap(annotatedParameterProcessors);
        for (RequestTemplate requestTemplate : requestTemplates) {
            requestTemplate.setUrl(shenyuClientFactoryBean.getUrl());
            requestTemplate.setName(shenyuClientFactoryBean.getName());
            requestTemplate.setContextId(Optional.ofNullable(shenyuClientFactoryBean.getContextId()).orElse(shenyuClientFactoryBean.getName()));
            if (StringUtils.hasText(shenyuClientFactoryBean.getPath())) {
                requestTemplate.setPath(shenyuClientFactoryBean.getPath() + requestTemplate.getPath());
            }
            methodHandlerMap.put(requestTemplate.getMethod(),
                    new ShenyuClientMethodHandler(shenyuClient, requestTemplate, shenyuSdkClient, annotatedArgumentProcessors));
        }
    }

    private Map<Class<? extends Annotation>, AnnotatedParameterProcessor> toAnnotatedArgumentProcessorMap(
            final Collection<AnnotatedParameterProcessor> processors) {
        Map<Class<? extends Annotation>, AnnotatedParameterProcessor> result = new HashMap<>();
        for (AnnotatedParameterProcessor processor : processors) {
            result.put(processor.getAnnotationType(), processor);
        }
        return result;
    }

    private FallbackFactory<?> getFallbackFactory(final Class<?> fallback, final Class<?> fallbackFactory) {
        try {
            if (fallback != void.class) {
                Object fallbackInstance = applicationContext.getBean(fallback);
                return new FallbackFactory.Default<>(fallbackInstance);
            } else if (fallbackFactory != void.class) {
                return (FallbackFactory<?>) applicationContext.getBean(fallbackFactory);
            }
            return null;
        } catch (BeansException exception) {
            LOG.error("No fallback available ", exception);
            return null;
        }
    }
}
