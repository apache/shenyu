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

import io.netty.channel.ChannelOption;
import org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyPlugin;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocol;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.subscriber.CommonAiProxyApiKeyDataSubscriber;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransportProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.netty.http.client.HttpClient;

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
     * @param aiProxyConfigService the aiProxyConfigService
     * @param aiProxyEngine        the aiProxyEngine
     * @param aiProxyPluginHandler the aiProxyPluginHandler
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin aiProxyPlugin(
            final AiProxyConfigService aiProxyConfigService,
            final AiProxyEngine aiProxyEngine,
            final AiProxyPluginHandler aiProxyPluginHandler) {
        return new AiProxyPlugin(
                aiProxyConfigService,
                aiProxyEngine,
                aiProxyPluginHandler);
    }

    /**
     * Ai proxy plugin handler.
     *
     * @return the shenyu plugin handler
     */
    @Bean
    public AiProxyPluginHandler aiProxyPluginHandler() {
        return new AiProxyPluginHandler();
    }

    @Bean
    public AiProxyConfigService aiProxyConfigService() {
        return new AiProxyConfigService();
    }

    /**
     * Ai proxy api key data subscriber.
     *
     * @return the subscriber
     */
    @Bean
    public AiProxyApiKeyDataSubscriber aiProxyApiKeyDataSubscriber() {
        return new CommonAiProxyApiKeyDataSubscriber();
    }

    /**
     * Ai proxy engine.
     *
     * @param aiProxyProtocolFactory protocol factory
     * @param aiProxyProviderFactory provider factory
     * @param aiProxyTransport       transport
     * @return the AI proxy engine
     */
    @Bean
    public AiProxyEngine aiProxyEngine(final AiProxyProtocolFactory aiProxyProtocolFactory,
                                       final AiProxyProviderFactory aiProxyProviderFactory, final AiProxyTransport aiProxyTransport) {
        return new AiProxyEngine(aiProxyProtocolFactory, aiProxyProviderFactory, aiProxyTransport);
    }

    /**
     * OpenAI chat protocol.
     *
     * @return the OpenAI chat protocol
     */
    @Bean
    public OpenAiChat openAiChat() {
        return new OpenAiChat();
    }

    /**
     * OpenAI Responses protocol backed by Connect conversion to Chat upstreams.
     *
     * @return the OpenAI Responses protocol
     */
    @Bean
    public OpenAiResponses openAiResponses() {
        return new OpenAiResponses();
    }

    /**
     * Ai proxy protocol factory.
     *
     * @param protocols protocols
     * @return the AI proxy protocol factory
     */
    @Bean
    public AiProxyProtocolFactory aiProxyProtocolFactory(final List<AiProxyProtocol> protocols) {
        return new AiProxyProtocolFactory(protocols);
    }

    /**
     * OpenAI provider.
     *
     * @return the OpenAI provider
     */
    @Bean(name = "openai")
    public OpenAi openAi() {
        return new OpenAi();
    }

    /**
     * Ai proxy provider factory.
     *
     * @param providers providers
     * @return the AI proxy provider factory
     */
    @Bean
    public AiProxyProviderFactory aiProxyProviderFactory(final List<AiProxyProvider> providers) {
        return new AiProxyProviderFactory(providers);
    }

    /**
     * Ai proxy transport.
     *
     * @param properties AI proxy transport properties
     * @return the AI proxy transport
     */
    @Bean
    @ConditionalOnMissingBean(AiProxyTransport.class)
    public AiProxyTransport aiProxyTransport(final AiProxyTransportProperties properties) {
        final WebClient webClient = WebClient.builder()
                .clientConnector(new ReactorClientHttpConnector(createHttpClient(properties)))
                .build();
        return new WebClientAiProxyTransport(webClient, properties);
    }


    /**
     * Create the generic OpenAI-compatible provider profile.
     *
     * @return provider profile
     */
    @Bean
    public OpenAiCompatible openAiCompatibleProvider() {
        return new OpenAiCompatible("OpenAICompatible");
    }

    /**
     * Create the DeepSeek compatible provider profile.
     *
     * @return provider profile
     */
    @Bean
    public OpenAiCompatible deepSeekProvider() {
        return new OpenAiCompatible("DeepSeek");
    }

    /**
     * Create the ALiYun compatible provider profile.
     *
     * @return provider profile
     */
    @Bean
    public OpenAiCompatible aLiYunProvider() {
        return new OpenAiCompatible("ALiYun");
    }

    /**
     * Create the OpenAPI compatible provider profile.
     *
     * @return provider profile
     */
    @Bean
    public OpenAiCompatible openAPIProvider() {
        return new OpenAiCompatible("OpenAPI");
    }

    /**
     * Create the Moonshot compatible provider profile.
     *
     * @return provider profile
     */
    @Bean
    public OpenAiCompatible moonshotProvider() {
        return new OpenAiCompatible("Moonshot");
    }

    /**
     * AI proxy transport properties.
     *
     * @return AI proxy transport properties
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.plugins.ai.proxy.transport")
    public AiProxyTransportProperties aiProxyTransportProperties() {
        return new AiProxyTransportProperties();
    }

    HttpClient createHttpClient(final AiProxyTransportProperties properties) {
        return HttpClient.create()
                .option(
                        ChannelOption.CONNECT_TIMEOUT_MILLIS,
                        connectTimeoutMillis(properties)
                );
    }

    int connectTimeoutMillis(final AiProxyTransportProperties properties) {
        return Math.toIntExact(properties.getConnectTimeout().toMillis());
    }
}
