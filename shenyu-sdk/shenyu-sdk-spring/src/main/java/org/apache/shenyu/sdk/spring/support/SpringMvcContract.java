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

package org.apache.shenyu.sdk.spring.support;

import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.spring.ShenyuClientFactoryBean;
import org.apache.shenyu.sdk.spring.factory.Contract;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.ResourceLoader;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;

import static org.springframework.core.annotation.AnnotatedElementUtils.findMergedAnnotation;

/**
 * SpringMvcContract.
 */
public class SpringMvcContract extends Contract.BaseContract {

    private static final String ACCEPT = "Accept";

    private static final String CONTENT_TYPE = "Content-Type";

    private ResourceLoader resourceLoader = new DefaultResourceLoader();

    @Override
    public RequestTemplate parseRequestTemplate(final Method method, final ShenyuClientFactoryBean shenyuClientFactoryBean) {
        final RequestTemplate requestTemplate = new RequestTemplate();
        requestTemplate.setMethod(method);
        requestTemplate.setReturnType(method.getReturnType());
        for (final Annotation methodAnnotation : method.getAnnotations()) {
            this.processAnnotationOnMethod(requestTemplate, methodAnnotation, method, shenyuClientFactoryBean);
        }
        return requestTemplate;
    }

    protected void processAnnotationOnMethod(final RequestTemplate requestTemplate, final Annotation methodAnnotation,
                                             final Method method, final ShenyuClientFactoryBean shenyuClientFactoryBean) {

        if (!RequestMapping.class.isInstance(methodAnnotation)
                && !methodAnnotation.annotationType().isAnnotationPresent(RequestMapping.class)) {
            return;
        }

        RequestMapping methodMapping = findMergedAnnotation(method, RequestMapping.class);
        // HTTP Method
        RequestMethod[] methods = methodMapping.method();
        if (methods.length == 0) {
            methods = new RequestMethod[] {RequestMethod.GET};
        }
        checkOne(method, methods, "method");

        requestTemplate.setHttpMethod(ShenyuRequest.HttpMethod.valueOf(methods[0].name()));

        // path
        checkAtMostOne(method, methodMapping.value(), "value");
        if (methodMapping.value().length > 0) {
            String pathValue = methodMapping.value()[0];
            if (pathValue != null && !pathValue.isEmpty()) {
                pathValue = resolve(pathValue);
                // Append path from @RequestMapping if value is present on method
                if (!pathValue.startsWith("/")
                        && StringUtils.hasText(shenyuClientFactoryBean.getPath())
                        && !shenyuClientFactoryBean.getPath().endsWith("/")) {
                    pathValue = "/" + pathValue;
                }
                requestTemplate.setPath(pathValue);
            }
        }

        // produces
        parseProduces(requestTemplate, methodMapping);

        // consumes
        parseConsumes(requestTemplate, methodMapping);

        // headers
        parseHeaders(requestTemplate, methodMapping);

    }

    private void parseProduces(final RequestTemplate requestTemplate, final RequestMapping annotation) {
        String[] serverProduces = annotation.produces();
        String clientAccepts = serverProduces.length == 0 ? null : serverProduces[0].isEmpty() ? null : serverProduces[0];
        if (clientAccepts != null) {
            requestTemplate.getHeaders().put(ACCEPT, Collections.singleton(clientAccepts));
        }
    }

    private void parseConsumes(final RequestTemplate requestTemplate, final RequestMapping annotation) {
        String[] serverConsumes = annotation.consumes();
        String clientProduces = serverConsumes.length == 0 ? null : serverConsumes[0].isEmpty() ? null : serverConsumes[0];
        if (clientProduces != null) {
            requestTemplate.getHeaders().put(CONTENT_TYPE, Collections.singleton(clientProduces));
        }
    }

    private void parseHeaders(final RequestTemplate requestTemplate, final RequestMapping annotation) {
        if (annotation.headers().length > 0) {
            for (String header : annotation.headers()) {
                int index = header.indexOf('=');
                if (!header.contains("!=") && index >= 0) {
                    requestTemplate.getHeaders().put(resolve(header.substring(0, index)),
                            Collections.singleton(resolve(header.substring(index + 1).trim())));
                }
            }
        }
    }

    private String resolve(final String value) {
        if (StringUtils.hasText(value) && resourceLoader instanceof ConfigurableApplicationContext) {
            return ((ConfigurableApplicationContext) resourceLoader).getEnvironment().resolvePlaceholders(value);
        }
        return value;
    }

    private void checkOne(final Method method, final Object[] values, final String fieldName) {
        Assert.state(values != null && values.length == 1,
                String.format("Method %s can only contain 1 %s field. Found: %s",
                        method.getName(), fieldName, values == null ? null : Arrays.asList(values)));
    }

    private void checkAtMostOne(final Method method, final Object[] values, final String fieldName) {
        Assert.state(values != null && (values.length == 0 || values.length == 1),
                String.format("Method %s can only contain at most 1 %s field. Found: %s",
                        method.getName(), fieldName, Arrays.asList(values)));
    }

    @Override
    public void setResourceLoader(final ResourceLoader resourceLoader) {
        this.resourceLoader = resourceLoader;
    }
}
