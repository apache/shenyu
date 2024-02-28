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

package org.apache.shenyu.springboot.starter.plugin.cryptor;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRequestPluginDataHandler;
import org.apache.shenyu.plugin.cryptor.handler.CryptorResponsePluginDataHandler;
import org.apache.shenyu.plugin.cryptor.plugin.CryptorRequestPlugin;
import org.apache.shenyu.plugin.cryptor.plugin.CryptorResponsePlugin;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.ServerCodecConfigurer;

/**
 *  Cryptor plugin.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.cryptor.enabled"}, havingValue = "true", matchIfMissing = true)
public class CryptorPluginConfiguration {

    /**
     * Cryptor request plugin.
     *
     * @param configurer configurer
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin cryptorRequestPlugin(final ServerCodecConfigurer configurer) {
        return new CryptorRequestPlugin(configurer.getReaders());
    }

    /**
     * Cryptor response plugin.
     *
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin cryptorResponsePlugin() {
        return new CryptorResponsePlugin();
    }

    /**
     * Cryptor request plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler cryptorRequestPluginDataHandler() {
        return new CryptorRequestPluginDataHandler();
    }

    /**
     * Cryptor response plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler cryptorResponsePluginDataHandler() {
        return new CryptorResponsePluginDataHandler();
    }
}
