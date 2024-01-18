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

package org.apache.shenyu.springboot.starter.plugin.motan;

import org.apache.shenyu.plugin.motan.MotanPlugin;
import org.apache.shenyu.plugin.motan.context.MotanShenyuContextDecorator;
import org.apache.shenyu.plugin.motan.handler.MotanMetaDataHandler;
import org.apache.shenyu.plugin.motan.handler.MotanPluginDataHandler;
import org.apache.shenyu.plugin.motan.proxy.MotanProxyService;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;

/**
 * The type motan plugin configuration.
 */
//@Configuration
//@ConditionalOnClass(MotanPlugin.class)
//@ConditionalOnProperty(value = {"shenyu.plugins.motan.enabled"}, havingValue = "true", matchIfMissing = true)
public class MotanPluginConfiguration implements BeanFactoryAware {

    @Override
    public void setBeanFactory(final BeanFactory beanFactory) throws BeansException {
        DefaultListableBeanFactory defaultListableBeanFactory = (DefaultListableBeanFactory) beanFactory;
        MotanProxyService motanProxyService = new MotanProxyService();
        defaultListableBeanFactory.registerSingleton("motanProxyService", motanProxyService);
        defaultListableBeanFactory.registerSingleton("motanPlugin", new MotanPlugin(motanProxyService));
        defaultListableBeanFactory.registerSingleton("motanPluginDataHandler", new MotanPluginDataHandler());
        defaultListableBeanFactory.registerSingleton("motanMetaDataHandler", new MotanMetaDataHandler());
        defaultListableBeanFactory.registerSingleton("motanShenyuContextDecorator", new MotanShenyuContextDecorator());
    }
}
