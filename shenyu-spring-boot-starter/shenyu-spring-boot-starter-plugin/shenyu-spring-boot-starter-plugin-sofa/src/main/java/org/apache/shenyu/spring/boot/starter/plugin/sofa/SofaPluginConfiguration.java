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

package org.apache.shenyu.spring.boot.starter.plugin.sofa;

import org.apache.shenyu.plugin.api.SoulPlugin;
import org.apache.shenyu.plugin.api.context.SoulContextDecorator;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.sofa.SofaPlugin;
import org.apache.shenyu.plugin.sofa.context.SofaSoulContextDecorator;
import org.apache.shenyu.plugin.sofa.handler.SofaPluginDataHandler;
import org.apache.shenyu.plugin.sofa.param.SofaBodyParamResolveServiceImpl;
import org.apache.shenyu.plugin.sofa.proxy.SofaProxyService;
import org.apache.shenyu.plugin.sofa.response.SofaResponsePlugin;
import org.apache.shenyu.plugin.sofa.subscriber.SofaMetaDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type sofa plugin configuration.
 *
 * @author tydhot
 */
@Configuration
@ConditionalOnClass(SofaPlugin.class)
public class SofaPluginConfiguration {

    /**
     * Sofa plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin sofaPlugin() {
        return new SofaPlugin(new SofaProxyService(new SofaBodyParamResolveServiceImpl()));
    }

    /**
     * Dubbo response plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin sofaResponsePlugin() {
        return new SofaResponsePlugin();
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
     * Sofa soul context decorator soul context decorator.
     *
     * @return the soul context decorator
     */
    @Bean
    public SoulContextDecorator sofaSoulContextDecorator() {
        return new SofaSoulContextDecorator();
    }
}
