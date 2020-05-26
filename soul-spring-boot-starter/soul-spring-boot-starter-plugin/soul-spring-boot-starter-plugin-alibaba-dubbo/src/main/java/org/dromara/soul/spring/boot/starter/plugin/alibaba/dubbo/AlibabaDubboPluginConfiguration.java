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

package org.dromara.soul.spring.boot.starter.plugin.alibaba.dubbo;

import org.dromara.soul.extend.api.dubbo.DubboParamResolveService;
import org.dromara.soul.extend.impl.dubbo.DefaultDubboParamResolveService;
import org.dromara.soul.plugin.alibaba.dubbo.AlibabaDubboPlugin;
import org.dromara.soul.plugin.alibaba.dubbo.body.BodyParamPlugin;
import org.dromara.soul.plugin.alibaba.dubbo.proxy.AlibabaDubboProxyService;
import org.dromara.soul.plugin.alibaba.dubbo.response.DubboResponsePlugin;
import org.dromara.soul.plugin.alibaba.dubbo.subscriber.AlibabaDubboMetaDataSubscriber;
import org.dromara.soul.plugin.alibaba.dubbo.subscriber.AlibabaDubboPluginDataSubscriber;
import org.dromara.soul.plugin.api.SoulPlugin;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.SearchStrategy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Alibaba dubbo plugin configuration.
 *
 * @author xiaoyu(Myth)
 */
@Configuration
@ConditionalOnClass(AlibabaDubboPlugin.class)
public class AlibabaDubboPluginConfiguration {
    
    /**
     * Dubbo plugin soul plugin.
     *
     * @param dubboParamResolveService the dubbo param resolve service
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin dubboPlugin(final DubboParamResolveService dubboParamResolveService) {
        return new AlibabaDubboPlugin(new AlibabaDubboProxyService(dubboParamResolveService));
    }
    
    /**
     * Body param plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin bodyParamPlugin() {
        return new BodyParamPlugin();
    }
    
    /**
     * Generic param resolve service dubbo param resolve service.
     *
     * @return the dubbo param resolve service
     */
    @Bean
    @ConditionalOnMissingBean(value = DubboParamResolveService.class, search = SearchStrategy.ALL)
    public DubboParamResolveService genericParamResolveService() {
        return new DefaultDubboParamResolveService();
    }
    
    /**
     * Dubbo response plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin dubboResponsePlugin() {
        return new DubboResponsePlugin();
    }
    
    /**
     * Data subscriber plugin data subscriber.
     *
     * @return the plugin data subscriber
     */
    @Bean
    public PluginDataSubscriber pluginDataSubscriber(){
        return new AlibabaDubboPluginDataSubscriber();
    }
    
    /**
     * Meta data subscriber meta data subscriber.
     *
     * @return the meta data subscriber
     */
    @Bean
    public MetaDataSubscriber metaDataSubscriber(){
        return new AlibabaDubboMetaDataSubscriber();
    }
}
