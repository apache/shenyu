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

import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.extractor.ApiBeansExtractor;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Controller;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

public class SpringMvcApiBeansExtractor implements ApiBeansExtractor {

    private final String contextPath;

    public SpringMvcApiBeansExtractor(final String contextPath) {
        this.contextPath = contextPath;
    }

    @Override
    public List<ApiBean> extract(final ApplicationContext applicationContext) {
        Map<String, Object> beanMap = applicationContext.getBeansWithAnnotation(Controller.class);

        List<ApiBean> apiBeans = new ArrayList<>();

        beanMap.forEach((k, v) -> {
            bean2ApiBean(k, v).ifPresent(apiBeans::add);
        });
        return apiBeans;
    }

    private Optional<ApiBean> bean2ApiBean(final String beanName, final Object bean) {

        Class<?> targetClass = getCorrectedClass(bean);

        RequestMapping classRequestMapping = AnnotationUtils.findAnnotation(targetClass, RequestMapping.class);

        String beanPath = Objects.isNull(classRequestMapping) ? "" : getPath(classRequestMapping);

        ApiBean apiBean = new ApiBean(contextPath, beanName, bean, beanPath, targetClass);

        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(targetClass);

        for (Method method : methods) {

            final RequestMapping methodRequestMapping =
                    AnnotatedElementUtils.findMergedAnnotation(method, RequestMapping.class);
            if (Objects.isNull(methodRequestMapping)) {
                continue;
            }
            apiBean.addApiDefinition(method, getPath(methodRequestMapping));
        }

        return Optional.of(apiBean);
    }

    private Class<?> getCorrectedClass(final Object bean) {

        Class<?> clazz = bean.getClass();
        if (AopUtils.isAopProxy(bean)) {
            clazz = AopUtils.getTargetClass(bean);
        }
        return clazz;
    }

    private String getPath(@NonNull final RequestMapping requestMapping) {
        return Optional.of(requestMapping.path()[0]).orElse("");
    }
}
