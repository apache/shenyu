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

package org.apache.shenyu.client.core.register;

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

    private final Class<?> beanClass;

    private final List<ApiDefinition> apiDefinitions = new ArrayList<>();

    public ApiBean(final String contextPath, final String beanName, final T beanInstance, final String beanPath, final Class<?> beanClass) {

        this.contextPath = contextPath;

        this.beanName = beanName;

        this.beanInstance = beanInstance;

        this.beanPath = beanPath;

        this.beanClass = beanClass;
    }

    /**
     * Adds apiDefinition to apiBean.
     *
     * @param method     apiMethod
     * @param methodPath methodPath
     */
    public void addApiDefinition(final Method method, final String methodPath) {
        ApiDefinition apiDefinition = new ApiDefinition(method, methodPath);
        apiDefinitions.add(apiDefinition);
    }

    /**
     * Gets apiDefinitions.
     *
     * @return ApiDefinitions.
     */
    public List<ApiDefinition> getApiDefinitions() {
        return apiDefinitions;
    }

    /**
     * Gets bean instance.
     *
     * @return bean Instance
     */
    public T getBeanInstance() {
        return beanInstance;
    }

    /**
     * Gets bean name.
     *
     * @return bean name.
     */
    public String getBeanName() {
        return beanName;
    }

    /**
     * Gets bean class.
     *
     * @return bean class
     */
    public Class<?> getBeanClass() {
        return beanClass;
    }

    /**
     * Gets bean path.
     *
     * @return bean path
     */
    public String getBeanPath() {
        return beanPath;
    }

    /**
     * Gets annotation from Bean Class.
     *
     * @param annotationClass annotation class
     * @param <A>             class extend Annotation
     * @return annotation
     */
    public <A extends Annotation> A getAnnotation(final Class<A> annotationClass) {
        return AnnotatedElementUtils.findMergedAnnotation(beanClass, annotationClass);
    }

    /**
     * Gets context path.
     *
     * @return context path
     */
    public String getContextPath() {
        return contextPath;
    }

    public final class ApiDefinition {

        private final Method apiMethod;

        private final String methodPath;

        private ApiDefinition(final Method apiMethod, final String methodPath) {
            this.apiMethod = apiMethod;
            this.methodPath = methodPath;
        }

        /**
         * Get Api Method.
         *
         * @return method.
         */
        public Method getApiMethod() {
            return apiMethod;
        }

        /**
         * Gets api method Name.
         *
         * @return api method Name.
         */
        public String getApiMethodName() {
            return apiMethod.getName();
        }

        /**
         * Gets bean path.
         *
         * @return bean path.
         */
        public String getBeanPath() {
            return beanPath;
        }

        /**
         * Gets api path.
         *
         * @return api path.
         */
        public String getApiPath() {
            return PathUtils.pathJoin(contextPath, beanPath, methodPath);
        }

        /**
         * Gets method path.
         *
         * @return method path
         */
        public String getMethodPath() {
            return methodPath;
        }

        /**
         * Gets bean class.
         *
         * @return bean class
         */
        public Class<?> getBeanClass() {
            return beanClass;
        }

        /**
         * Gets api bean.
         *
         * @return api bean
         */
        public ApiBean<T> getApiBean() {
            return ApiBean.this;
        }

        /**
         * Gets context path.
         *
         * @return context path
         */
        public String getContextPath() {
            return contextPath;
        }

        /**
         * Get the annotation from Method.
         *
         * @param annotationClass annotationClass
         * @param <A>             annotationClass extends Annotation
         * @return annotation
         */
        public <A extends Annotation> A getAnnotation(final Class<A> annotationClass) {
            return AnnotatedElementUtils.findMergedAnnotation(apiMethod, annotationClass);
        }
    }
}
