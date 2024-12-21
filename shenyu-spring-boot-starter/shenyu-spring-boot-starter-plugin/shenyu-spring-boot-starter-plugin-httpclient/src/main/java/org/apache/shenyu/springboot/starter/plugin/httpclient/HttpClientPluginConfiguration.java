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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.httpclient.NettyHttpClientPlugin;
import org.apache.shenyu.plugin.httpclient.WebClientPlugin;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.reactive.function.client.ExchangeStrategies;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.netty.http.client.HttpClient;
import reactor.netty.resources.LoopResources;

import java.util.Objects;

/**
 * The type Http client plugin configuration.
 */
@Configuration
public class HttpClientPluginConfiguration {

    /**
     * Http client properties.
     *
     * @return the http client properties
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.httpclient")
    public HttpClientProperties httpClientProperties() {
        return new HttpClientProperties();
    }

    /**
     * Http client loop resource.
     *
     * @param properties the properties
     * @return the http client loop resource
     */
    @Bean
    @ConditionalOnProperty("shenyu.httpclient.thread-pool.prefix")
    public LoopResources httpClientLoopResource(final HttpClientProperties properties) {
        HttpClientProperties.ThreadPool threadPool = properties.getThreadPool();
        return LoopResources.create(threadPool.getPrefix(), threadPool.getSelectCount(),
                threadPool.getWorkerCount(), threadPool.getDaemon());
    }

    /**
     * Shenyu http client.
     *
     * @param properties the properties
     * @param provider   the loop resources bean provider
     * @param serverProperties the server properties
     * @return the http client
     */
    @Bean
    @ConditionalOnMissingBean({HttpClient.class, HttpClientFactory.class})
    public HttpClientFactory httpClient(final HttpClientProperties properties,
                                        final ObjectProvider<LoopResources> provider,
                                        final ServerProperties serverProperties) {
        return new HttpClientFactory(properties, provider.getIfAvailable(), serverProperties);
    }


    /**
     * The type Web client configuration.
     */
    @Configuration
    @ConditionalOnProperty(name = "shenyu.httpclient.strategy", havingValue = "webClient")
    static class WebClientConfiguration {

        /**
         * Web client plugin shenyu plugin.
         *
         * @param httpClient the http client
         * @return the shenyu plugin
         */
        @Bean
        public ShenyuPlugin webClientPlugin(
                final HttpClientProperties properties,
                final ObjectProvider<HttpClient> httpClient) {
            WebClient webClient = WebClient.builder()
                    // fix Exceeded limit on max bytes to buffer
                    // detail see https://stackoverflow.com/questions/59326351/configure-spring-codec-max-in-memory-size-when-using-reactiveelasticsearchclient
                    .exchangeStrategies(ExchangeStrategies.builder()
                            .codecs(codecs -> codecs.defaultCodecs().maxInMemorySize(properties.getMaxInMemorySize() * Constants.BYTES_PER_MB))
                            .build())
                    .clientConnector(new ReactorClientHttpConnector(Objects.requireNonNull(httpClient.getIfAvailable())))
                    .build();
            return new WebClientPlugin(webClient);
        }
    }

    /**
     * The type Netty http client configuration.
     */
    @Configuration
    @ConditionalOnProperty(name = "shenyu.httpclient.strategy", havingValue = "netty", matchIfMissing = true)
    static class NettyHttpClientConfiguration {

        /**
         * Netty http client plugin.
         *
         * @param httpClient the http client
         * @return the shenyu plugin
         */
        @Bean
        public ShenyuPlugin nettyHttpClientPlugin(final ObjectProvider<HttpClient> httpClient) {
            return new NettyHttpClientPlugin(httpClient.getIfAvailable());
        }
    }
}
