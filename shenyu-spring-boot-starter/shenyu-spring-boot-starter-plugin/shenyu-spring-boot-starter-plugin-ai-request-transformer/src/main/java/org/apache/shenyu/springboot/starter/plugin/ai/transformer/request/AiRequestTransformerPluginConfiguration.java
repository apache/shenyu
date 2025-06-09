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

package org.apache.shenyu.springboot.starter.plugin.ai.transformer.request;

import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.factory.DeepSeekModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.factory.OpenAiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.request.AiRequestTransformerPlugin;
import org.apache.shenyu.plugin.ai.transformer.request.handler.AiRequestTransformerPluginHandler;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.ServerCodecConfigurer;

import java.util.List;

/**
 * The type ai Request Transformer plugin configuration.
 */

@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.ai.transformer.request.enabled"}, havingValue = "true", matchIfMissing = true)
public class AiRequestTransformerPluginConfiguration {

    /**
     * Ai Request Transformer plugin.
     *
     * @param configurer the configurer
     * @param aiModelFactoryList the aiModelFactoryList
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin aiProxyPlugin(final ServerCodecConfigurer configurer, final List<AiModelFactory> aiModelFactoryList) {
        return new AiRequestTransformerPlugin(configurer.getReaders(), aiModelFactoryRegistry(aiModelFactoryList));
    }

    /**
     * Ai Request Transformer plugin handler.
     *
     * @param aiModelFactoryList the aiModelFactoryList
     * @return the shenyu plugin handler
     */
    @Bean
    public AiRequestTransformerPluginHandler aiProxyPluginHandler(final List<AiModelFactory> aiModelFactoryList) {
        return new AiRequestTransformerPluginHandler(aiModelFactoryRegistry(aiModelFactoryList));
    }

    /**
     * Ai model factory registry.
     *
     * @param aiModelFactoryList aiModelFactoryList
     * @return the registry
     */
    @Bean
    public AiModelFactoryRegistry aiModelFactoryRegistry(final List<AiModelFactory> aiModelFactoryList) {
        return new AiModelFactoryRegistry(aiModelFactoryList);
    }

    /**
     * OpenAi model factory.
     *
     * @return the factory
     */
    @Bean
    public OpenAiModelFactory openAiModelFactory() {
        return new OpenAiModelFactory();
    }

    /**
     * DeepSeek model factory.
     *
     * @return the factory
     */
    @Bean
    public DeepSeekModelFactory deepSeekModelFactory() {
        return new DeepSeekModelFactory();
    }

}
