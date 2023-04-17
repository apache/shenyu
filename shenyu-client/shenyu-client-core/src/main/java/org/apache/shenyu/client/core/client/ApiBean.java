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

package org.apache.shenyu.client.core.client;

import org.apache.shenyu.common.utils.PathUtils;
import org.springframework.core.annotation.AnnotatedElementUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class ApiBean<T> {

    private final String beanName;

    private final T beanInstance;

    private final String beanPath;

    private final String contextPath;

    private final Class<?> targetClass;

    List<ApiDefinition> apiDefinitions = new ArrayList<>();

    public ApiBean(String contextPath, String beanName, T beanInstance, String beanPath, Class<?> targetClass) {

        this.contextPath = contextPath;

        this.beanName = beanName;

        this.beanInstance = beanInstance;

        this.beanPath = beanPath;

        this.targetClass = targetClass;
    }

    public ApiDefinition addApiDefinition(Method method, String methodPath) {
        ApiDefinition apiDefinition = new ApiDefinition(method, methodPath);
        apiDefinitions.add(apiDefinition);
        return apiDefinition;
    }

    public List<ApiDefinition> getApiDefinitions() {
        return apiDefinitions;
    }

    public T getBeanInstance() {
        return beanInstance;
    }

    public String getBeanName() {
        return beanName;
    }

    public Class<?> getTargetClass() {
        return targetClass;
    }

    public String getBeanPath() {
        return beanPath;
    }

    public <A extends Annotation> A getAnnotation(Class<A> annotationClass) {
        return AnnotatedElementUtils.findMergedAnnotation(targetClass, annotationClass);
    }

    public String getContextPath() {
        return contextPath;
    }

    public class ApiDefinition {

        private final Method apiMethod;

        private final String methodPath;

        private ApiDefinition(Method apiMethod, String methodPath) {
            this.apiMethod = apiMethod;
            this.methodPath = methodPath;
        }

        public Method getApiMethod() {
            return apiMethod;
        }

        public String getApiMethodName() {
            return apiMethod.getName();
        }

        public String getParentPath() {
            return beanPath;
        }

        public String getApiPath() {
            return PathUtils.pathJoin(contextPath, beanPath, methodPath);
        }

        public String getMethodPath() {
            return methodPath;
        }

        public Class<?> getBeanClass() {
            return targetClass;
        }

        public ApiBean<T> getApiBean() {
            return ApiBean.this;
        }

        public String getContextPath() {
            return contextPath;
        }

        public <A extends Annotation> A getAnnotation(Class<A> annotationClass) {
            return AnnotatedElementUtils.findMergedAnnotation(apiMethod, annotationClass);
        }
    }
}
