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

package org.apache.shenyu.springboot.starter.plugin.sofa;

import org.apache.shenyu.plugin.sofa.SofaPlugin;
import org.apache.shenyu.plugin.sofa.context.SofaShenyuContextDecorator;
import org.apache.shenyu.plugin.sofa.handler.SofaMetaDataHandler;
import org.apache.shenyu.plugin.sofa.handler.SofaPluginDataHandler;
import org.apache.shenyu.plugin.sofa.param.SofaParamResolveServiceImpl;
import org.apache.shenyu.plugin.sofa.proxy.SofaProxyService;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;

/**
 * The type sofa plugin configuration.
 */
@Configuration
@ConditionalOnClass(SofaPlugin.class)
@ConditionalOnProperty(value = {"shenyu.plugins.sofa.enabled"}, havingValue = "true", matchIfMissing = true)
public class SofaPluginConfiguration implements BeanFactoryAware {

    @Override
    public void setBeanFactory(final BeanFactory beanFactory) throws BeansException {
        DefaultListableBeanFactory defaultListableBeanFactory = (DefaultListableBeanFactory) beanFactory;
        defaultListableBeanFactory.registerSingleton("sofaShenyuContextDecorator", new SofaShenyuContextDecorator());
        defaultListableBeanFactory.registerSingleton("sofaMetaDataHandler", new SofaMetaDataHandler());
        defaultListableBeanFactory.registerSingleton("sofaPluginDataHandler", new SofaPluginDataHandler());
        final SofaParamResolveServiceImpl sofaParamResolveService = new SofaParamResolveServiceImpl();
        defaultListableBeanFactory.registerSingleton("sofaParamResolveService", sofaParamResolveService);
        defaultListableBeanFactory.registerSingleton("sofaPlugin", new SofaPlugin(new SofaProxyService(sofaParamResolveService)));
    }
}
