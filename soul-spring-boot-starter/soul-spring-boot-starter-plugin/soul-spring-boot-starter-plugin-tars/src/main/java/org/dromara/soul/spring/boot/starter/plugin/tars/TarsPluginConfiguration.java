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

package org.dromara.soul.spring.boot.starter.plugin.tars;

import org.dromara.soul.plugin.api.SoulPlugin;
import org.dromara.soul.plugin.base.handler.PluginDataHandler;
import org.dromara.soul.plugin.tars.TarsPlugin;
import org.dromara.soul.plugin.tars.handler.TarsPluginDataHandler;
import org.dromara.soul.plugin.tars.param.BodyParamPlugin;
import org.dromara.soul.plugin.tars.response.TarsResponsePlugin;
import org.dromara.soul.plugin.tars.subscriber.TarsMetaDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type tars plugin configuration.
 *
 * @author tydhot
 */
@Configuration
@ConditionalOnClass(TarsPlugin.class)
public class TarsPluginConfiguration {

    /**
     * Tars plugin soul plugin.
     *
     * @return the tars plugin
     */
    @Bean
    public SoulPlugin tarsPlugin() {
        return new TarsPlugin();
    }

    /**
     * Body param plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin tarsBodyParamPlugin() {
        return new BodyParamPlugin();
    }

    /**
     * Tars response plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin tarsResponsePlugin() {
        return new TarsResponsePlugin();
    }

    /**
     * Tars meta data subscriber meta data subscriber.
     *
     * @return the meta data subscriber
     */
    @Bean
    public MetaDataSubscriber tarsMetaDataSubscriber() {
        return new TarsMetaDataSubscriber();
    }

    /**
     * Tars plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler tarsPluginDataHandler() {
        return new TarsPluginDataHandler();
    }
}
