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

package org.apache.shenyu.springboot.plugin.divide;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.divide.DividePlugin;
import org.apache.shenyu.plugin.divide.context.DivideShenyuContextDecorator;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.divide.handler.DivideMetaDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * ShenyuConfiguration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.divide.enabled"}, havingValue = "true", matchIfMissing = true)
public class DividePluginConfiguration {

    /**
     * init dividePlugin.
     *
     * @return {@linkplain DividePlugin}
     */
    @Bean
    public ShenyuPlugin dividePlugin() {
        return new DividePlugin();
    }

    /**
     * Divide plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler dividePluginDataHandler() {
        return new DividePluginDataHandler();
    }

    /**
     * divide meta data handler.
     *
     * @return the meta data handler
     */
    @Bean
    public MetaDataHandler divideMetaDataHandler() {
        return new DivideMetaDataHandler();
    }

    /**
     * Divide shenyu context decorator.
     *
     * @return the shenyu context decorator
     */
    @Bean
    public ShenyuContextDecorator divideShenyuContextDecorator() {
        return new DivideShenyuContextDecorator();
    }
}
