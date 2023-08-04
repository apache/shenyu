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

import org.springframework.aop.support.AopUtils;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * An API bean maps a collection of API pairs whose information comes from a class.
 */
public class ApiBean {
    
    /**
     * client name.<br>
     * In general, it refers to the client type (RPC type), and users distinguish between multiple clients.
     */
    private final String clientName;
    
    /**
     * bean Instance name.<br>
     * It can be a bean name for spring, it can be a Java variable name.
     */
    private final String beanName;
    
    /**
     * bean instance.
     */
    private final Object beanInstance;
    
    /**
     * bean class.
     */
    private final Class<?> beanClass;
    
    /**
     * API collection.
     */
    private final List<ApiDefinition> apiDefinitions;
    
    /**
     * supper uri.
     */
    private String beanPath = "";
    
    /**
     * Custom properties for beans.
     */
    private final Properties beanProperties = new Properties();
    
    /**
     * status.
     */
    private Status status = Status.INIT;
    
    public ApiBean(@NonNull final String clientName, @NonNull final String beanName, @NonNull final Object beanInstance, @NonNull final List<ApiDefinition> apiDefinitions) {
        this.clientName = clientName;
        this.beanName = beanName;
        this.beanInstance = beanInstance;
        this.beanClass = getCorrectedClass(beanInstance);
        this.apiDefinitions = apiDefinitions;
        for (ApiDefinition apiDefinition : apiDefinitions) {
            apiDefinition.apiBean = this;
        }
    }
    
    public ApiBean(@NonNull final String clientName, @NonNull final String beanName, @NonNull final Object beanInstance) {
        this.clientName = clientName;
        this.beanName = beanName;
        this.beanInstance = beanInstance;
        this.beanClass = getCorrectedClass(beanInstance);
        this.apiDefinitions = new ArrayList<>(5);
    }
    
    public ApiBean(@NonNull final String clientName, @NonNull final String beanName, @NonNull final Object beanInstance, final String beanPath) {
        this.clientName = clientName;
        this.beanName = beanName;
        this.beanInstance = beanInstance;
        this.beanClass = getCorrectedClass(beanInstance);
        this.beanPath = beanPath;
        this.apiDefinitions = new ArrayList<>(5);
    }
    
    /**
     * Adds apiDefinition to apiBean.
     *
     * @param method     apiMethod
     * @param methodPath methodPath
     */
    public void addApiDefinition(final Method method, final String methodPath) {
        ApiDefinition apiDefinition = new ApiDefinition(this, method, methodPath);
        apiDefinitions.add(apiDefinition);
    }
    
    /**
     * get clientName.
     *
     * @return clientName
     */
    public String getClientName() {
        return clientName;
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
    public Object getBeanInstance() {
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
     * add properties value.
     *
     * @param name  name
     * @param value value
     */
    public void addProperties(final String name, final String value) {
        beanProperties.put(name, value);
    }
    
    /**
     * get properties.
     *
     * @param name name
     * @return value
     */
    public String getPropertiesValue(final String name) {
        return beanProperties.getProperty(name);
    }
    
    /**
     * get status.
     *
     * @return status
     */
    public Status getStatus() {
        return status;
    }
    
    /**
     * set status.
     *
     * @param status status
     */
    public void setStatus(final Status status) {
        this.status = status;
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
    
    private Class<?> getCorrectedClass(final Object bean) {
        Class<?> clazz = bean.getClass();
        if (AopUtils.isAopProxy(bean)) {
            clazz = AopUtils.getTargetClass(bean);
        }
        return clazz;
    }
    
    /**
     * set beanPath.
     *
     * @param beanPath beanPath
     */
    public void setBeanPath(final String beanPath) {
        this.beanPath = beanPath;
    }
    
    /**
     * deep copy.
     *
     * @return ApiBean
     */
    public ApiBean copy() {
        final ApiBean copy = new ApiBean(clientName, beanName, beanInstance, beanPath);
        beanProperties.forEach((k, v) -> copy.addProperties(k.toString(), Objects.toString(v)));
        for (ApiDefinition definition : apiDefinitions) {
            final ApiDefinition newDefinition = new ApiDefinition(this, definition.apiMethod, definition.methodPath);
            definition.apiProperties.forEach((k, v) -> newDefinition.addProperties(k.toString(), Objects.toString(v)));
            copy.apiDefinitions.add(newDefinition);
        }
        return copy;
    }
    
    /**
     * API corresponds to an accessible interface proxy service, which can be an http service, RPC service, TCP service.
     */
    
    public static final class ApiDefinition {
        
        /**
         * Function information for the service.
         */
        private final Method apiMethod;
        
        /**
         * The instance of the class in which the function resides.
         */
        private ApiBean apiBean;
        
        /**
         * The URI path after conversion to the HTTP service.
         */
        private String methodPath;
        
        /**
         * api custom properties.<br>
         * The properties of each client are different.
         */
        private final Properties apiProperties = new Properties();
        
        /**
         * status.
         */
        private Status status = Status.INIT;
        
        public ApiDefinition(final Method apiMethod) {
            this.apiMethod = apiMethod;
            this.methodPath = apiMethod.getName();
        }
        
        public ApiDefinition(final ApiBean apiBean, final Method apiMethod) {
            this.apiBean = apiBean;
            this.apiMethod = apiMethod;
        }
        
        private ApiDefinition(final ApiBean apiBean, final Method apiMethod, final String methodPath) {
            this.apiBean = apiBean;
            this.apiMethod = apiMethod;
            this.methodPath = methodPath;
        }
        
        /**
         * set methodPath.
         *
         * @param methodPath methodPath
         */
        public void setMethodPath(final String methodPath) {
            this.methodPath = methodPath;
        }
        
        /**
         * add properties value.
         *
         * @param name  name
         * @param value value
         */
        public void addProperties(final String name, final String value) {
            apiProperties.put(name, value);
        }
        
        /**
         * get properties.
         *
         * @param name name
         * @return value
         */
        public String getPropertiesValue(final String name) {
            return apiProperties.getProperty(name);
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
            return apiBean.beanPath;
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
            return apiBean.beanClass;
        }
        
        /**
         * Gets api bean.
         *
         * @return api bean
         */
        public ApiBean getApiBean() {
            return apiBean;
        }
        
        /**
         * get status.
         *
         * @return staus
         */
        public Status getStatus() {
            return status;
        }
        
        /**
         * set status.
         *
         * @param status status
         */
        public void setStatus(final Status status) {
            this.status = status;
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
        
        /**
         * Gets parameterTypes.
         *
         * @return parameterTypes
         */
        public String getParameterTypes() {
            return Optional.ofNullable(apiMethod)
                    .map(m -> Arrays.stream(m.getParameterTypes())
                            .map(Class::getName)
                            .collect(Collectors.joining(",")))
                    .orElse(null);
        }
    }
    
    public enum Status {
        
        /**
         * init status.
         */
        INIT,
        
        /**
         * Cannot be registered.
         */
        CAN_NO_BE_REGISTERED,
        
        /**
         * Registrable.
         */
        REGISTRABLE,
        
        /**
         * Registrable(only api).
         * <br>
         * Only register APIs included in the bean.
         */
        REGISTRABLE_API,
        
        /**
         * Registrable(only bean).
         * <br>
         * A registrable bean means that the bean needs to be registered, ignoring the APIs in it.
         */
        REGISTRABLE_BEAN,
        
        /**
         * Already registered.
         * <br>
         * Status that has already been registered should be skipped
         */
        REGISTERED
    }
}
