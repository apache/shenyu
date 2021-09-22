/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.apache.shenyu.springboot.starter.plugin.sofa;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.sofa.SofaPlugin;
import org.apache.shenyu.plugin.sofa.context.SofaShenyuContextDecorator;
import org.apache.shenyu.plugin.sofa.handler.SofaPluginDataHandler;
import org.apache.shenyu.plugin.sofa.param.SofaParamResolveService;
import org.apache.shenyu.plugin.sofa.param.SofaParamResolveServiceImpl;
import org.apache.shenyu.plugin.sofa.proxy.SofaProxyService;
import org.apache.shenyu.plugin.sofa.subscriber.SofaMetaDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.SearchStrategy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type sofa plugin configuration.
 */
@Configuration
@ConditionalOnClass(SofaPlugin.class)
public class SofaPluginConfiguration {
    
    /**
     * Sofa plugin shenyu plugin.
     *
     * @param sofaParamResolveService the sofa param resolve service
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin sofaPlugin(final ObjectProvider<SofaParamResolveService> sofaParamResolveService) {
        return new SofaPlugin(new SofaProxyService(sofaParamResolveService.getIfAvailable()));
    }
    
    /**
     * Sofa param resolve service sofa param resolve service.
     *
     * @return the sofa param resolve service
     */
    @Bean
    @ConditionalOnMissingBean(value = SofaParamResolveService.class, search = SearchStrategy.ALL)
    public SofaParamResolveService sofaParamResolveService() {
        return new SofaParamResolveServiceImpl();
    }
    
    /**
     * Sofa plugin data handler plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler sofaPluginDataHandler() {
        return new SofaPluginDataHandler();
    }
    
    /**
     * Sofa meta data subscriber meta data subscriber.
     *
     * @return the meta data subscriber
     */
    @Bean
    public MetaDataSubscriber sofaMetaDataSubscriber() {
        return new SofaMetaDataSubscriber();
    }
    
    /**
     * Sofa shenyu context decorator shenyu context decorator.
     *
     * @return the shenyu context decorator
     */
    @Bean
    public ShenyuContextDecorator sofaShenyuContextDecorator() {
        return new SofaShenyuContextDecorator();
    }
}
