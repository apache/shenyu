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

package org.apache.shenyu.springboot.starter.plugin.ai.proxy;

import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.factory.DeepSeekModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.factory.OpenAiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyPlugin;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.ChatClientCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyExecutorService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.subscriber.CommonAiProxyApiKeyDataSubscriber;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;
    
/** 
 * The type ai proxy plugin configuration.
 */
@Configuration
@ConditionalOnProperty(
        value = {"shenyu.plugins.ai.proxy.enabled"},
        havingValue = "true",
        matchIfMissing = true)
public class AiProxyPluginConfiguration {

    /**
     * Ai proxy plugin.
     *
     * @param aiModelFactoryRegistry the aiModelFactoryRegistry
     * @param aiProxyConfigService the aiProxyConfigService
     * @param aiProxyExecutorService the aiProxyExecutorService
     * @param chatClientCache the chatClientCache
     * @param aiProxyPluginHandler the aiProxyPluginHandler
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin aiProxyPlugin(
            final AiModelFactoryRegistry aiModelFactoryRegistry,
            final AiProxyConfigService aiProxyConfigService,
            final AiProxyExecutorService aiProxyExecutorService,
            final ChatClientCache chatClientCache,
            final AiProxyPluginHandler aiProxyPluginHandler) {
        return new AiProxyPlugin(
                aiModelFactoryRegistry,
                aiProxyConfigService,
                aiProxyExecutorService,
                chatClientCache,
                aiProxyPluginHandler);
    }

    /**
     * Ai proxy plugin handler.
     *
     * @param chatClientCache the chatClientCache
     * @return the shenyu plugin handler
     */
    @Bean
    public AiProxyPluginHandler aiProxyPluginHandler(final ChatClientCache chatClientCache) {
        return new AiProxyPluginHandler(chatClientCache);
    }

    @Bean
    public ChatClientCache chatClientCache() {
        return new ChatClientCache();
    }

    @Bean
    public AiProxyConfigService aiProxyConfigService() {
        return new AiProxyConfigService();
    }

    @Bean
    public AiProxyExecutorService aiProxyExecutorService() {
        return new AiProxyExecutorService();
    }

    /**
     * Ai model factory registry.
     *
     * @param aiModelFactoryList aiModelFactoryList
     * @return the registry
     */
    @Bean
    public AiModelFactoryRegistry aiModelFactoryRegistry(
            final List<AiModelFactory> aiModelFactoryList) {
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

    /**
     * Ai proxy api key data subscriber.
     *
     * @param chatClientCache the chatClientCache
     * @return the subscriber
     */
    @Bean
    public AiProxyApiKeyDataSubscriber aiProxyApiKeyDataSubscriber(final ChatClientCache chatClientCache) {
        return new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
    }
}
