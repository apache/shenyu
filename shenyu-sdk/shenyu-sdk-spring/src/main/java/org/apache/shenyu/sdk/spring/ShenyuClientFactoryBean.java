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

package org.apache.shenyu.sdk.spring;

import org.apache.shenyu.sdk.spring.proxy.ShenyuClientProxyFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * ShenyuClientFactoryBean.
 */
public class ShenyuClientFactoryBean implements FactoryBean<Object>, InitializingBean, ApplicationContextAware, BeanFactoryAware {

    private Class<?> type;

    private String name;

    private String url;

    private String contextId;

    private String path;

    private ApplicationContext applicationContext;

    private BeanFactory beanFactory;

    private Class<?> fallback = void.class;

    private Class<?> fallbackFactory = void.class;

    @Override
    public void afterPropertiesSet() {
        Assert.hasText(contextId, "Context id must be set");
        Assert.hasText(name, "Name must be set");
    }

    @Override
    public Object getObject() {
        return getTarget();
    }

    /**
     * getTarget.
     *
     * @return {@link Object}
     */
    private Object getTarget() {
        if (StringUtils.hasText(url) && !url.startsWith("http")) {
            url = "http://" + url;
        }
        return ShenyuClientProxyFactory.createProxy(type, applicationContext, this);
    }

    @Override
    public Class<?> getObjectType() {
        return type;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

    /**
     * type.
     *
     * @return Type
     */
    public Class<?> getType() {
        return type;
    }

    /**
     * set type.
     *
     * @param type type
     */
    public void setType(final Class<?> type) {
        this.type = type;
    }

    /**
     * name.
     *
     * @return Name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * url end not With "/".
     *
     * @return Url
     */
    public String getUrl() {
        return url;
    }

    /**
     * set url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * contextId.
     *
     * @return ContextId
     */
    public String getContextId() {
        return contextId;
    }

    /**
     * set contextId.
     *
     * @param contextId contextId
     */
    public void setContextId(final String contextId) {
        this.contextId = contextId;
    }

    /**
     * path startsWith '/' .
     *
     * @return Path
     */
    public String getPath() {
        return path;
    }

    /**
     * set path.
     *
     * @param path path
     */
    public void setPath(final String path) {
        this.path = path;
    }

    /**
     * applicationContext.
     *
     * @return ApplicationContext
     */
    public ApplicationContext getApplicationContext() {
        return applicationContext;
    }

    /**
     * beanFactory.
     *
     * @return BeanFactory
     */
    public BeanFactory getBeanFactory() {
        return beanFactory;
    }

    /**
     * fallback.
     *
     * @return Fallback
     */
    public Class<?> getFallback() {
        return fallback;
    }

    /**
     * set fallback.
     *
     * @param fallback fallback
     */
    public void setFallback(final Class<?> fallback) {
        this.fallback = fallback;
    }

    /**
     * fallbackFactory.
     *
     * @return FallbackFactory
     */
    public Class<?> getFallbackFactory() {
        return fallbackFactory;
    }

    /**
     * set fallbackFactory.
     *
     * @param fallbackFactory fallbackFactory
     */
    public void setFallbackFactory(final Class<?> fallbackFactory) {
        this.fallbackFactory = fallbackFactory;
    }

    @Override
    public void setBeanFactory(final BeanFactory beanFactory) throws BeansException {
        this.beanFactory = beanFactory;
    }

    @Override
    public void setApplicationContext(final ApplicationContext context) throws BeansException {
        applicationContext = context;
        beanFactory = context;
    }
}
