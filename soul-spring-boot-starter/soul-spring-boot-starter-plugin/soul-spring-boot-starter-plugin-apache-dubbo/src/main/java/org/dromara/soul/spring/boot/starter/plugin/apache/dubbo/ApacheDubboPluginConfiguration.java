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

package org.dromara.soul.spring.boot.starter.plugin.apache.dubbo;

import org.dromara.soul.plugin.apache.dubbo.ApacheDubboPlugin;
import org.dromara.soul.plugin.apache.dubbo.handler.ApacheDubboPluginDataHandler;
import org.dromara.soul.plugin.apache.dubbo.param.BodyParamPlugin;
import org.dromara.soul.plugin.apache.dubbo.proxy.ApacheDubboProxyService;
import org.dromara.soul.plugin.apache.dubbo.response.DubboResponsePlugin;
import org.dromara.soul.plugin.apache.dubbo.subscriber.ApacheDubboMetaDataSubscriber;
import org.dromara.soul.plugin.api.SoulPlugin;
import org.dromara.soul.plugin.api.dubbo.DubboParamResolveService;
import org.dromara.soul.plugin.base.handler.PluginDataHandler;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Apache dubbo plugin configuration.
 *
 * @author xiaoyu(Myth)
 */
@Configuration
@ConditionalOnClass(ApacheDubboPlugin.class)
public class ApacheDubboPluginConfiguration {

    /**
     * Dubbo plugin soul plugin.
     *
     * @param dubboParamResolveService the dubbo param resolve service
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin apacheDubboPlugin(final ObjectProvider<DubboParamResolveService> dubboParamResolveService) {
        return new ApacheDubboPlugin(new ApacheDubboProxyService(dubboParamResolveService.getIfAvailable()));
    }

    /**
     * Body param plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin apacheDubboBodyParamPlugin() {
        return new BodyParamPlugin();
    }

    /**
     * Dubbo response plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin apacheDubboResponsePlugin() {
        return new DubboResponsePlugin();
    }

    /**
     * Apache dubbo plugin data handler plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler apacheDubboPluginDataHandler() {
        return new ApacheDubboPluginDataHandler();
    }

    /**
     * Apache dubbo meta data subscriber meta data subscriber.
     *
     * @return the meta data subscriber
     */
    @Bean
    public MetaDataSubscriber apacheDubboMetaDataSubscriber() {
        return new ApacheDubboMetaDataSubscriber();
    }
}
