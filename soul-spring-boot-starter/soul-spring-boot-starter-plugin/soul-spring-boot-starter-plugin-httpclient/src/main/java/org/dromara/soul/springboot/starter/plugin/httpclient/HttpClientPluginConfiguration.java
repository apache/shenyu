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

package org.dromara.soul.springboot.starter.plugin.httpclient;

import io.netty.channel.ChannelOption;
import io.netty.handler.ssl.SslContextBuilder;
import io.netty.handler.ssl.util.InsecureTrustManagerFactory;
import org.dromara.soul.plugin.api.SoulPlugin;
import org.dromara.soul.plugin.httpclient.NettyHttpClientPlugin;
import org.dromara.soul.plugin.httpclient.WebClientPlugin;
import org.dromara.soul.plugin.httpclient.config.HttpClientProperties;
import org.dromara.soul.plugin.httpclient.response.NettyClientResponsePlugin;
import org.dromara.soul.plugin.httpclient.response.WebClientResponsePlugin;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.PropertyMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.util.StringUtils;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.netty.http.client.HttpClient;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.tcp.ProxyProvider;

import java.security.cert.X509Certificate;
import java.util.Objects;

/**
 * The type Http client plugin configuration.
 *
 * @author xiaoyu
 */
@Configuration
public class HttpClientPluginConfiguration {
    
    /**
     * Http client properties http client properties.
     *
     * @return the http client properties
     */
    @Bean
    @ConfigurationProperties(prefix = "soul.httpclient")
    public HttpClientProperties httpClientProperties() {
        return new HttpClientProperties();
    }
    
    /**
     * Gateway http client http client.
     *
     * @param properties the properties
     * @return the http client
     */
    @Bean
    public HttpClient httpClient(final HttpClientProperties properties) {
        // configure pool resources
        HttpClientProperties.Pool pool = properties.getPool();
        ConnectionProvider connectionProvider;
        if (pool.getType() == HttpClientProperties.Pool.PoolType.DISABLED) {
            connectionProvider = ConnectionProvider.newConnection();
        } else if (pool.getType() == HttpClientProperties.Pool.PoolType.FIXED) {
            connectionProvider = ConnectionProvider.fixed(pool.getName(),
                    pool.getMaxConnections(), pool.getAcquireTimeout());
        } else {
            connectionProvider = ConnectionProvider.elastic(pool.getName());
        }
        HttpClient httpClient = HttpClient.create(connectionProvider)
                .tcpConfiguration(tcpClient -> {
                    if (properties.getConnectTimeout() != null) {
                        tcpClient = tcpClient.option(
                                ChannelOption.CONNECT_TIMEOUT_MILLIS,
                                properties.getConnectTimeout());
                    }
                    // configure proxy if proxy host is set.
                    HttpClientProperties.Proxy proxy = properties.getProxy();
                    if (StringUtils.hasText(proxy.getHost())) {
                        tcpClient = tcpClient.proxy(proxySpec -> {
                            ProxyProvider.Builder builder = proxySpec
                                    .type(ProxyProvider.Proxy.HTTP)
                                    .host(proxy.getHost());
                            PropertyMapper map = PropertyMapper.get();
                            map.from(proxy::getPort).whenNonNull().to(builder::port);
                            map.from(proxy::getUsername).whenHasText()
                                    .to(builder::username);
                            map.from(proxy::getPassword).whenHasText()
                                    .to(password -> builder.password(s -> password));
                            map.from(proxy::getNonProxyHostsPattern).whenHasText()
                                    .to(builder::nonProxyHosts);
                        });
                    }
                    tcpClient = tcpClient.option(ChannelOption.SO_TIMEOUT, 5000);
                    return tcpClient;
                });

        HttpClientProperties.Ssl ssl = properties.getSsl();
        if (ssl.getTrustedX509CertificatesForTrustManager().length > 0
                || ssl.isUseInsecureTrustManager()) {
            httpClient = httpClient.secure(sslContextSpec -> {
                // configure ssl
                SslContextBuilder sslContextBuilder = SslContextBuilder.forClient();
                X509Certificate[] trustedX509Certificates = ssl
                        .getTrustedX509CertificatesForTrustManager();
                if (trustedX509Certificates.length > 0) {
                    sslContextBuilder.trustManager(trustedX509Certificates);
                } else if (ssl.isUseInsecureTrustManager()) {
                    sslContextBuilder
                            .trustManager(InsecureTrustManagerFactory.INSTANCE);
                }
                sslContextSpec.sslContext(sslContextBuilder)
                        .defaultConfiguration(ssl.getDefaultConfigurationType())
                        .handshakeTimeout(ssl.getHandshakeTimeout())
                        .closeNotifyFlushTimeout(ssl.getCloseNotifyFlushTimeout())
                        .closeNotifyReadTimeout(ssl.getCloseNotifyReadTimeout());
            });
        }

        if (properties.isWiretap()) {
            httpClient = httpClient.wiretap(true);
        }

        return httpClient;
    }
    
    /**
     * The type Web client configuration.
     */
    @Configuration
    @ConditionalOnProperty(name = "soul.httpclient.strategy", havingValue = "webClient", matchIfMissing = true)
    static class WebClientConfiguration {
    
        /**
         * Web client plugin soul plugin.
         *
         * @param httpClient the http client
         * @return the soul plugin
         */
        @Bean
        public SoulPlugin webClientPlugin(final ObjectProvider<HttpClient> httpClient) {
            WebClient webClient = WebClient.builder()
                    .clientConnector(new ReactorClientHttpConnector(Objects.requireNonNull(httpClient.getIfAvailable())))
                    .build();
            return new WebClientPlugin(webClient);
        }
    
        /**
         * Web client response plugin soul plugin.
         *
         * @return the soul plugin
         */
        @Bean
        public SoulPlugin webClientResponsePlugin() {
            return new WebClientResponsePlugin();
        }

    }
    
    /**
     * The type Netty http client configuration.
     */
    @Configuration
    @ConditionalOnProperty(name = "soul.httpclient.strategy", havingValue = "netty")
    static class NettyHttpClientConfiguration {
    
        /**
         * Netty http client plugin soul plugin.
         *
         * @param httpClient the http client
         * @return the soul plugin
         */
        @Bean
        public SoulPlugin nettyHttpClientPlugin(final ObjectProvider<HttpClient> httpClient) {
            return new NettyHttpClientPlugin(httpClient.getIfAvailable());
        }
    
        /**
         * Netty client response plugin soul plugin.
         *
         * @return the soul plugin
         */
        @Bean
        public SoulPlugin nettyClientResponsePlugin() {
            return new NettyClientResponsePlugin();
        }

    }

}
