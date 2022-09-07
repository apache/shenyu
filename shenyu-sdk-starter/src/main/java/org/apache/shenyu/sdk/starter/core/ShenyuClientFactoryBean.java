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

package org.apache.shenyu.sdk.starter.core;

import org.apache.shenyu.sdk.starter.core.factory.ShenyuClientProxyFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import java.util.Objects;

/**
 * ShenyuClientFactoryBean.
 */
public class ShenyuClientFactoryBean implements FactoryBean<Object>, InitializingBean, ApplicationContextAware, BeanFactoryAware {

	private Class<?> type;

	private String name;

	private String url;

	private String contextId;

	private String path;

	private boolean decode404;

	private boolean inheritParentContext = true;

	private ApplicationContext applicationContext;

	private BeanFactory beanFactory;

	private Class<?> fallback = void.class;

	private Class<?> fallbackFactory = void.class;

	private boolean refreshableClient = false;


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

	public Class<?> getType() {
		return type;
	}

	public void setType(Class<?> type) {
		this.type = type;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getContextId() {
		return contextId;
	}

	public void setContextId(String contextId) {
		this.contextId = contextId;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public boolean isDecode404() {
		return decode404;
	}

	public void setDecode404(boolean decode404) {
		this.decode404 = decode404;
	}


	public ApplicationContext getApplicationContext() {
		return applicationContext;
	}

	@Override
	public void setApplicationContext(ApplicationContext context) throws BeansException {
		applicationContext = context;
		beanFactory = context;
	}

	public Class<?> getFallback() {
		return fallback;
	}

	public void setFallback(Class<?> fallback) {
		this.fallback = fallback;
	}

	public Class<?> getFallbackFactory() {
		return fallbackFactory;
	}

	public void setFallbackFactory(Class<?> fallbackFactory) {
		this.fallbackFactory = fallbackFactory;
	}

	public void setRefreshableClient(boolean refreshableClient) {
		this.refreshableClient = refreshableClient;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		ShenyuClientFactoryBean that = (ShenyuClientFactoryBean) o;
		return Objects.equals(applicationContext, that.applicationContext)
				&& Objects.equals(beanFactory, that.beanFactory) && decode404 == that.decode404
				&& inheritParentContext == that.inheritParentContext && Objects.equals(fallback, that.fallback)
				&& Objects.equals(fallbackFactory, that.fallbackFactory) && Objects.equals(name, that.name)
				&& Objects.equals(path, that.path) && Objects.equals(type, that.type) && Objects.equals(url, that.url)
				&& Objects.equals(refreshableClient, that.refreshableClient);
	}

	@Override
	public int hashCode() {
		return Objects.hash(applicationContext, beanFactory, decode404, inheritParentContext, fallback, fallbackFactory,
				name, path, type, url, refreshableClient);
	}

	@Override
	public String toString() {
		return new StringBuilder("ShenyuClientFactoryBean{").append("type=").append(type).append(", ").append("name='")
				.append(name).append("', ").append("url='").append(url).append("', ").append("path='").append(path)
				.append("', ").append("decode404=").append(decode404).append(", ").append("inheritParentContext=")
				.append(inheritParentContext).append(", ").append("applicationContext=").append(applicationContext)
				.append(", ").append("beanFactory=").append(beanFactory).append(", ").append("fallback=")
				.append(fallback).append(", ").append("fallbackFactory=").append(fallbackFactory).append("}")
				.append("refreshableClient=").append(refreshableClient).append("}").toString();
	}

	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

}
