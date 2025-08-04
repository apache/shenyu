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

package org.apache.shenyu.springboot.starter.plugin.ai.transformer.response;

import org.apache.shenyu.plugin.ai.transformer.response.AiResponseTransformerPlugin;
import org.apache.shenyu.plugin.ai.transformer.response.handler.AiResponseTransformerPluginHandler;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.HttpMessageReader;

import java.util.List;

/**
 * Ai Response Transformer Plugin Configuration.
 */
@Configuration
public class AiResponseTransformerPluginConfiguration {

    /**
     * Ai response transformer plugin.
     *
     * @param messageReaders the message readers
     * @param aiModelFactoryRegistry the ai model factory registry
     * @return the ai response transformer plugin
     */
    @Bean
    public AiResponseTransformerPlugin aiResponseTransformerPlugin(final List<HttpMessageReader<?>> messageReaders,
                                                                   final AiModelFactoryRegistry aiModelFactoryRegistry) {
        return new AiResponseTransformerPlugin(messageReaders, aiModelFactoryRegistry);
    }

    /**
     * Ai response transformer plugin handler.
     *
     * @param aiModelFactoryRegistry the ai model factory registry
     * @return the ai response transformer plugin handler
     */
    @Bean
    public AiResponseTransformerPluginHandler aiResponseTransformerPluginHandler(final AiModelFactoryRegistry aiModelFactoryRegistry) {
        return new AiResponseTransformerPluginHandler(aiModelFactoryRegistry);
    }
} 