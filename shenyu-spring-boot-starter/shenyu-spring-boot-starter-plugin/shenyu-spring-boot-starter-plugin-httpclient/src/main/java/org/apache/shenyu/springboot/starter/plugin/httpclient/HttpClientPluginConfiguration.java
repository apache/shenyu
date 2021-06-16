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

import io.netty.channel.ChannelOption;
import io.netty.handler.ssl.SslContextBuilder;
import io.netty.handler.ssl.util.InsecureTrustManagerFactory;
import io.netty.handler.timeout.ReadTimeoutHandler;
import io.netty.handler.timeout.WriteTimeoutHandler;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.httpclient.NettyHttpClientPlugin;
import org.apache.shenyu.plugin.httpclient.WebClientPlugin;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
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
import java.util.concurrent.TimeUnit;

/**
 * The type Http client plugin configuration.
 */
@Configuration
public class HttpClientPluginConfiguration {

    /**
     * Http client properties http client properties.
     *
     * @return the http client properties
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.httpclient")
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
                    // The write and read timeouts are serving as generic socket idle state handlers.
                    tcpClient = tcpClient.doOnConnected(c -> {
                        c.addHandlerLast(new WriteTimeoutHandler(properties.getWriteTimeout(), TimeUnit.MILLISECONDS));
                        c.addHandlerLast(new ReadTimeoutHandler(properties.getReadTimeout(), TimeUnit.MILLISECONDS));
                    });
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
    @ConditionalOnProperty(name = "shenyu.httpclient.strategy", havingValue = "webClient", matchIfMissing = true)
    static class WebClientConfiguration {

        /**
         * Web client plugin shenyu plugin.
         *
         * @param httpClient the http client
         * @return the shenyu plugin
         */
        @Bean
        public ShenyuPlugin webClientPlugin(final ObjectProvider<HttpClient> httpClient) {
            WebClient webClient = WebClient.builder()
                    .clientConnector(new ReactorClientHttpConnector(Objects.requireNonNull(httpClient.getIfAvailable())))
                    .build();
            return new WebClientPlugin(webClient);
        }
    }

    /**
     * The type Netty http client configuration.
     */
    @Configuration
    @ConditionalOnProperty(name = "shenyu.httpclient.strategy", havingValue = "netty")
    static class NettyHttpClientConfiguration {

        /**
         * Netty http client plugin shenyu plugin.
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
