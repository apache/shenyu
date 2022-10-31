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

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.motan.MotanPlugin;
import org.apache.shenyu.plugin.motan.context.MotanShenyuContextDecorator;
import org.apache.shenyu.plugin.motan.handler.MotanPluginDataHandler;
import org.apache.shenyu.plugin.motan.proxy.MotanProxyService;
import org.apache.shenyu.plugin.motan.handler.MotanMetaDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type motan plugin configuration.
 */
@Configuration
@ConditionalOnClass(MotanPlugin.class)
@ConditionalOnProperty(value = {"shenyu.plugins.motan.enabled"}, havingValue = "true", matchIfMissing = true)
public class MotanPluginConfiguration {

    /**
     * Motan proxy service.
     *
     * @return the motan proxy service
     */
    @Bean
    public MotanProxyService motanProxyService() {
        return new MotanProxyService();
    }

    /**
     * Motan plugin.
     *
     * @param motanProxyService the motan proxy service
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin motanPlugin(final MotanProxyService motanProxyService) {
        return new MotanPlugin(motanProxyService);
    }

    /**
     * Motan plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler motanPluginDataHandler() {
        return new MotanPluginDataHandler();
    }

    /**
     * Motan meta data handler.
     *
     * @return the meta data handler
     */
    @Bean
    public MetaDataHandler motanMetaDataHandler() {
        return new MotanMetaDataHandler();
    }
    
    /**
     * motan shenyu context decorator.
     *
     * @return the shenyu context decorator
     */
    @Bean
    public ShenyuContextDecorator motanShenyuContextDecorator() {
        return new MotanShenyuContextDecorator();
    }
}
