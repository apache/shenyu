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

import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyPlugin;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiExactResponseCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiResponseCacheProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiResponseCacheScopeResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverterFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.ConnectResponsesToChatConverter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocol;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiStreamCompletionProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiProviderOptions;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyAttemptListener;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyRetryProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.subscriber.CommonAiProxyApiKeyDataSubscriber;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransportProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.reactive.function.client.WebClient;

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
     * Bind the explicit Chat SSE completion policy; compatibility remains the default.
     * @return completion properties
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.ai.proxy.stream-completion")
    public AiStreamCompletionProperties aiStreamCompletionProperties() {
        return new AiStreamCompletionProperties();
    }

    /**
     * Ai proxy plugin.
     *
     * @param aiProxyConfigService the aiProxyConfigService
     * @param aiProxyEngine        the aiProxyEngine
     * @param aiProxyPluginHandler the aiProxyPluginHandler
     * @param observers optional application terminal sink
     * @param cacheProperties exact response cache properties
     * @param scopeResolvers optional application-owned cache authorization adapter
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin aiProxyPlugin(
            final AiProxyConfigService aiProxyConfigService,
            final AiProxyEngine aiProxyEngine,
            final AiProxyPluginHandler aiProxyPluginHandler, final ObjectProvider<AiCallObserver> observers,
            final AiResponseCacheProperties cacheProperties, final ObjectProvider<AiResponseCacheScopeResolver> scopeResolvers) {
        return new AiProxyPlugin(
                aiProxyConfigService,
                aiProxyEngine,
                aiProxyPluginHandler, observer(observers), cacheProperties.isEnabled() ? scopeResolvers.getIfAvailable() : null);
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
    public AiProxyConfigService aiProxyConfigService(final AiProxyPluginHandler aiProxyPluginHandler) {
        return new AiProxyConfigService(aiProxyPluginHandler);
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
     * @param aiProtocolRouteResolver protocol route resolver
     * @param aiProxyTransport       transport
     * @param observers optional application terminal sink
     * @param retryProperties        retry properties
     * @param attemptListener        optional attempt listener
     * @param responseCache          optional exact response cache
     * @return the AI proxy engine
     */
    @Bean
    public AiProxyEngine aiProxyEngine(final AiProxyProtocolFactory aiProxyProtocolFactory,
                                       final AiProxyProviderFactory aiProxyProviderFactory,
                                       final AiProtocolRouteResolver aiProtocolRouteResolver,
                                       final AiProxyTransport aiProxyTransport,
                                       final ObjectProvider<AiCallObserver> observers,
                                       final AiProxyRetryProperties retryProperties,
                                       final ObjectProvider<AiProxyAttemptListener> attemptListener,
                                       final ObjectProvider<AiExactResponseCache> responseCache) {
        return new AiProxyEngine(aiProxyProtocolFactory, aiProxyProviderFactory,
                aiProtocolRouteResolver, aiProxyTransport, retryProperties,
                observer(observers), attemptListener.getIfAvailable(() -> event -> {
                }), responseCache.getIfAvailable());
    }

    private AiCallObserver observer(final ObjectProvider<AiCallObserver> provider) {
        List<AiCallObserver> candidates = provider.stream().toList();
        if (candidates.size() > 1) {
            throw new IllegalStateException("AI terminal observation requires one unambiguous application sink");
        }
        return candidates.isEmpty() ? null : candidates.get(0);
    }

    /**
     * Responses-to-Chat protocol converter.
     *
     * @return the Responses-to-Chat protocol converter
     */
    @Bean
    @ConditionalOnMissingBean
    public ConnectResponsesToChatConverter connectResponsesToChatConverter() {
        return new ConnectResponsesToChatConverter();
    }

    /**
     * AI protocol converter factory.
     *
     * @param converters protocol converters
     * @return the AI protocol converter factory
     */
    @Bean
    @ConditionalOnMissingBean
    public AiProtocolConverterFactory aiProtocolConverterFactory(final List<AiProtocolConverter> converters) {
        return new AiProtocolConverterFactory(converters);
    }

    /**
     * AI protocol route resolver.
     *
     * @param converterFactory protocol converter factory
     * @return the AI protocol route resolver
     */
    @Bean
    @ConditionalOnMissingBean
    public AiProtocolRouteResolver aiProtocolRouteResolver(final AiProtocolConverterFactory converterFactory) {
        return new AiProtocolRouteResolver(converterFactory);
    }

    /**
     * OpenAI chat protocol.
     *
     * @return the OpenAI chat protocol
     */
    @Bean
    public OpenAiChat openAiChat() {
        return new OpenAiChat(aiStreamCompletionProperties().getMode());
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
     * Shared Reactor Netty resources for the default AI proxy transport.
     *
     * @param properties AI proxy transport properties
     * @return shared AI proxy HTTP client resources
     */
    @Bean(destroyMethod = "close")
    @ConditionalOnMissingBean({AiProxyTransport.class, AiProxyHttpClientResources.class})
    public AiProxyHttpClientResources aiProxyHttpClientResources(
            final AiProxyTransportProperties properties) {
        return new AiProxyHttpClientResources(properties);
    }

    /**
     * Default AI proxy transport.
     *
     * @param resources shared AI proxy HTTP client resources
     * @param properties AI proxy transport properties
     * @return AI proxy transport
     */
    @Bean
    @ConditionalOnMissingBean(AiProxyTransport.class)
    public AiProxyTransport aiProxyTransport(
            final AiProxyHttpClientResources resources,
            final AiProxyTransportProperties properties) {
        WebClient webClient = WebClient.builder()
                .clientConnector(
                        new ReactorClientHttpConnector(resources.getHttpClient()))
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
        return new OpenAiCompatible("DeepSeek", OpenAiProviderOptions.deepSeek());
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

    /**
     * Startup limits for the opt-in exact response cache.
     * @return cache properties
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.plugins.ai.proxy.response-cache")
    public AiResponseCacheProperties aiResponseCacheProperties() {
        return new AiResponseCacheProperties();
    }

    /**
     * Create a local cache only when explicitly enabled.
     * @param properties startup cache limits
     * @return bounded cache, cleared when the application context closes
     */
    @Bean(destroyMethod = "invalidateAll")
    @ConditionalOnProperty(name = "shenyu.plugins.ai.proxy.response-cache.enabled", havingValue = "true")
    public AiExactResponseCache aiExactResponseCache(final AiResponseCacheProperties properties) {
        return new AiExactResponseCache(properties);
    }

    /**
     * AI proxy retry properties.
     *
     * @return AI proxy retry properties
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.plugins.ai.proxy.retry")
    public AiProxyRetryProperties aiProxyRetryProperties() {
        return new AiProxyRetryProperties();
    }
}
