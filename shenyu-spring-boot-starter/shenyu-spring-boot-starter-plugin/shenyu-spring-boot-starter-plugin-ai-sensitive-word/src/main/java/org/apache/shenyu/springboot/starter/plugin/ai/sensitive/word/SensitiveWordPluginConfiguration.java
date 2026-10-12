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

package org.apache.shenyu.springboot.starter.plugin.ai.sensitive.word;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.ai.sensitive.word.SensitiveWordPlugin;
import org.apache.shenyu.plugin.ai.sensitive.word.handler.SensitiveWordPluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.ServerCodecConfigurer;

/**
 * The sensitive word plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = "shenyu.plugins.sensitive-word.enabled", havingValue = "true", matchIfMissing = true)
public class SensitiveWordPluginConfiguration {

    /**
     * the sensitive word plugin.
     *
     * @param configurer the server codec configurer
     * @return the sensitive word plugin
     */
    @Bean
    public ShenyuPlugin sensitiveWordPlugin(final ServerCodecConfigurer configurer) {
        return new SensitiveWordPlugin(configurer.getReaders());
    }

    /**
     * the sensitive word plugin data handler, it caches the redis client, the rule handles and the
     * dictionaries.
     *
     * @return the sensitive word plugin data handler
     */
    @Bean
    public PluginDataHandler sensitiveWordPluginDataHandler() {
        return new SensitiveWordPluginDataHandler();
    }
}
