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
import io.netty.handler.ssl.util.InsecureTrustManagerFactory;
import io.netty.handler.timeout.IdleStateHandler;
import io.netty.handler.timeout.ReadTimeoutHandler;
import io.netty.handler.timeout.WriteTimeoutHandler;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
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
import org.springframework.web.reactive.function.client.ExchangeStrategies;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.netty.http.client.HttpClient;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.DefaultSslContextSpec;
import reactor.netty.tcp.SslProvider;
import reactor.netty.transport.ProxyProvider;

import java.security.cert.X509Certificate;
import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

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
     * @return the http client
     */
    @Bean
    public HttpClient httpClient(final HttpClientProperties properties,
                                 final ObjectProvider<LoopResources> provider) {
        // configure pool resources.
        HttpClientProperties.Pool pool = properties.getPool();
        ConnectionProvider connectionProvider = buildConnectionProvider(pool);
        HttpClient httpClient = HttpClient.create(connectionProvider)
                .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, properties.getConnectTimeout());
        HttpClientProperties.Proxy proxy = properties.getProxy();
        if (StringUtils.isNotEmpty(proxy.getHost())) {
            httpClient = setHttpClientProxy(httpClient, proxy);
        }
        httpClient.doOnConnected(connection -> {
            connection.addHandlerLast(new IdleStateHandler(properties.getReaderIdleTime(), properties.getWriterIdleTime(), properties.getAllIdleTime(), TimeUnit.MILLISECONDS));
            connection.addHandlerLast(new WriteTimeoutHandler(properties.getWriteTimeout(), TimeUnit.MILLISECONDS));
            connection.addHandlerLast(new ReadTimeoutHandler(properties.getReadTimeout(), TimeUnit.MILLISECONDS));
        });
        final LoopResources loopResources = provider.getIfAvailable();
        if (Objects.nonNull(loopResources)) {
            httpClient.runOn(loopResources);
        }
        HttpClientProperties.Ssl ssl = properties.getSsl();
        if (StringUtils.isNotEmpty(ssl.getKeyStorePath())
                || ArrayUtils.isNotEmpty(ssl.getTrustedX509CertificatesForTrustManager())
                || ssl.isUseInsecureTrustManager()) {
            httpClient = httpClient.secure(sslContextSpec -> setSsl(sslContextSpec, ssl));
        }
        if (properties.isWiretap()) {
            httpClient = httpClient.wiretap(true);
        }
        // set to false, fix java.io.IOException: Connection reset by peer
        // see https://github.com/reactor/reactor-netty/issues/388
        return httpClient.keepAlive(properties.isKeepAlive());
    }

    /**
     * set http proxy.
     * @param httpClient http client
     * @param proxy proxy
     * @return HttpClient
     */
    private HttpClient setHttpClientProxy(final HttpClient httpClient, final HttpClientProperties.Proxy proxy) {
        return httpClient.proxy(proxySpec -> {
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

    private void setSsl(final SslProvider.SslContextSpec sslContextSpec, final HttpClientProperties.Ssl ssl) {
        SslProvider.ProtocolSslContextSpec spec = DefaultSslContextSpec.forClient()
                .configure(sslContextBuilder -> {
                    X509Certificate[] trustedX509Certificates = ssl.getTrustedX509CertificatesForTrustManager();
                    if (ArrayUtils.isNotEmpty(trustedX509Certificates)) {
                        sslContextBuilder.trustManager(trustedX509Certificates);
                    } else if (ssl.isUseInsecureTrustManager()) {
                        sslContextBuilder.trustManager(InsecureTrustManagerFactory.INSTANCE);
                    }
                    sslContextBuilder.keyManager(ssl.getKeyManagerFactory());
                    sslContextBuilder.sslProvider(ssl.getDefaultConfigurationType());
                });
        sslContextSpec.sslContext(spec)
                .handshakeTimeout(ssl.getHandshakeTimeout())
                .closeNotifyFlushTimeout(ssl.getCloseNotifyFlushTimeout())
                .closeNotifyReadTimeout(ssl.getCloseNotifyReadTimeout());
    }

    private ConnectionProvider buildConnectionProvider(final HttpClientProperties.Pool pool) {
        ConnectionProvider connectionProvider;
        if (pool.getType() == HttpClientProperties.Pool.PoolType.DISABLED) {
            connectionProvider = ConnectionProvider.newConnection();
        } else if (pool.getType() == HttpClientProperties.Pool.PoolType.FIXED) {
            // reactor remove fixed pool by fixed method from 0.9.4
            // reason: https://github.com/reactor/reactor-netty/issues/1499 and https://github.com/reactor/reactor-netty/issues/1960
            connectionProvider = buildFixedConnectionPool(pool.getName(), pool.getMaxConnections(), pool.getAcquireTimeout(), pool.getMaxIdleTime());
        } else {
            // please see https://projectreactor.io/docs/netty/release/reference/index.html#_connection_pool_2
            // reactor remove elastic pool by elastic method from 0.9.4
            // reason: https://github.com/reactor/reactor-netty/issues/1499 and https://github.com/reactor/reactor-netty/issues/1960
            connectionProvider = buildElasticConnectionPool(pool.getName(), pool.getMaxIdleTime());
        }
        return connectionProvider;
    }

    /**
     * build fixed connection pool.
     *
     * @param poolName pool name
     * @param maxConnections max connections
     * @param acquireTimeout pending acquire timeout
     * @param maxIdleTime max idle time
     * @return {@link ConnectionProvider}
     */
    public static ConnectionProvider buildFixedConnectionPool(final String poolName, final Integer maxConnections,
                                             final Long acquireTimeout, final Duration maxIdleTime) {
        if (maxConnections <= 0) {
            throw new IllegalArgumentException("Max Connections value must be strictly positive");
        }
        if (acquireTimeout < 0) {
            throw new IllegalArgumentException("Acquire Timeout value must be positive");
        }
        return ConnectionProvider.builder(poolName)
                .maxConnections(maxConnections)
                .pendingAcquireTimeout(Duration.ofMillis(acquireTimeout))
                .maxIdleTime(maxIdleTime)
                .build();
    }

    /**
     * build elastic connection provider pool.
     *
     * @param poolName pool name
     * @param maxIdleTime max idle time
     * @return {@link ConnectionProvider} elastic pool
     */
    public ConnectionProvider buildElasticConnectionPool(final String poolName, final Duration maxIdleTime) {
        // about the args, please see https://projectreactor.io/docs/netty/release/reference/index.html#_connection_pool_2
        return ConnectionProvider.builder(poolName)
                .maxConnections(Integer.MAX_VALUE)
                .pendingAcquireTimeout(Duration.ofMillis(0))
                .pendingAcquireMaxCount(-1)
                .maxIdleTime(maxIdleTime)
                .build();
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
        public ShenyuPlugin webClientPlugin(
                final HttpClientProperties properties,
                final ObjectProvider<HttpClient> httpClient) {
            WebClient webClient = WebClient.builder()
                    // fix Exceeded limit on max bytes to buffer
                    // detail see https://stackoverflow.com/questions/59326351/configure-spring-codec-max-in-memory-size-when-using-reactiveelasticsearchclient
                    .exchangeStrategies(ExchangeStrategies.builder()
                            .codecs(codecs -> codecs.defaultCodecs().maxInMemorySize(properties.getMaxInMemorySize() * 1024 * 1024))
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
    @ConditionalOnProperty(name = "shenyu.httpclient.strategy", havingValue = "netty")
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
