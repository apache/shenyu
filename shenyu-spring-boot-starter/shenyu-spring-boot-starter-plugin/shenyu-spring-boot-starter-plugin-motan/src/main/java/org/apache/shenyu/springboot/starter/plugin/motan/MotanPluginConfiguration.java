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

package org.apache.shenyu.springboot.starter.plugin.motan;

import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.motan.MotanPlugin;
import org.apache.shenyu.plugin.motan.context.MotanShenyuContextDecorator;
import org.apache.shenyu.plugin.motan.handler.MotanPluginDataHandler;
import org.apache.shenyu.plugin.motan.proxy.MotanProxyService;
import org.apache.shenyu.plugin.motan.subscriber.MotanMetaDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type motan plugin configuration.
 */
@Configuration
@ConditionalOnClass(MotanPlugin.class)
public class MotanPluginConfiguration {

    /**
     * Motan plugin shenyu plugin.
     *
     * @return the shenyu plugin
     */
    @Bean
    public MotanPlugin motanPlugin() {
        return new MotanPlugin(new MotanProxyService());
    }

    /**
     * Motan plugin data handler plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler motanPluginDataHandler() {
        return new MotanPluginDataHandler();
    }

    /**
     * Motan meta data subscriber meta data subscriber.
     *
     * @return the meta data subscriber
     */
    @Bean
    public MetaDataSubscriber motanMetaDataSubscriber() {
        return new MotanMetaDataSubscriber();
    }
    
    /**
     * motan shenyu context decorator shenyu context decorator.
     *
     * @return the shenyu context decorator
     */
    @Bean
    public ShenyuContextDecorator motanShenyuContextDecorator() {
        return new MotanShenyuContextDecorator();
    }
}
