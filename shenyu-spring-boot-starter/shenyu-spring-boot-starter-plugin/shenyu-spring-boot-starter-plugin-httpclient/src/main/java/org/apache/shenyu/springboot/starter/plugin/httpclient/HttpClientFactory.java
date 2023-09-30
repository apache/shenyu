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
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties.Pool;
import org.springframework.beans.factory.config.AbstractFactoryBean;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.context.properties.PropertyMapper;
import reactor.netty.http.Http11SslContextSpec;
import reactor.netty.http.Http2SslContextSpec;
import reactor.netty.http.HttpProtocol;
import reactor.netty.http.client.HttpClient;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.resources.ConnectionProvider.Builder;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.SslProvider;
import reactor.netty.transport.ProxyProvider;

import java.security.cert.X509Certificate;
import java.time.Duration;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public class HttpClientFactory extends AbstractFactoryBean<HttpClient> {

    private final HttpClientProperties properties;

    private final LoopResources loopResources;
    
    private final ServerProperties serverProperties;

    public HttpClientFactory(final HttpClientProperties httpClientProperties,
                             final LoopResources loopResources,
                             final ServerProperties serverProperties) {
        this.properties = httpClientProperties;
        this.loopResources = loopResources;
        this.serverProperties = serverProperties;
    }

    @Override
    public Class<?> getObjectType() {
        return HttpClient.class;
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
        SslProvider.ProtocolSslContextSpec clientSslContext = (serverProperties.getHttp2().isEnabled())
                ? Http2SslContextSpec.forClient() : Http11SslContextSpec.forClient();
        clientSslContext.configure(sslContextBuilder -> {
            X509Certificate[] trustedX509Certificates = ssl.getTrustedX509CertificatesForTrustManager();
            if (ArrayUtils.isNotEmpty(trustedX509Certificates)) {
                sslContextBuilder.trustManager(trustedX509Certificates);
            } else if (ssl.isUseInsecureTrustManager()) {
                sslContextBuilder.trustManager(InsecureTrustManagerFactory.INSTANCE);
            }
            sslContextBuilder.keyManager(ssl.getKeyManagerFactory());
        });
        sslContextSpec.sslContext(clientSslContext)
                .handshakeTimeout(ssl.getHandshakeTimeout())
                .closeNotifyFlushTimeout(ssl.getCloseNotifyFlushTimeout())
                .closeNotifyReadTimeout(ssl.getCloseNotifyReadTimeout());
    }

    @Override
    protected HttpClient createInstance() {
        // configure pool resources.
        HttpClientProperties.Pool pool = properties.getPool();
        ConnectionProvider connectionProvider = buildConnectionProvider(pool);
        HttpClient httpClient = HttpClient.create(connectionProvider)
                .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, properties.getConnectTimeout());
        if (serverProperties.getHttp2().isEnabled()) {
            httpClient = httpClient.protocol(HttpProtocol.HTTP11, HttpProtocol.H2);
        }
        HttpClientProperties.Proxy proxy = properties.getProxy();
        if (StringUtils.isNotEmpty(proxy.getHost())) {
            httpClient = setHttpClientProxy(httpClient, proxy);
        }
        httpClient.doOnConnected(connection -> {
            connection.addHandlerLast(new IdleStateHandler(properties.getReaderIdleTime(), properties.getWriterIdleTime(), properties.getAllIdleTime(), TimeUnit.MILLISECONDS));
            connection.addHandlerLast(new WriteTimeoutHandler(properties.getWriteTimeout(), TimeUnit.MILLISECONDS));
            connection.addHandlerLast(new ReadTimeoutHandler(properties.getReadTimeout(), TimeUnit.MILLISECONDS));
        });
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

    private ConnectionProvider buildConnectionProvider(final HttpClientProperties.Pool pool) {
        if (pool.getType() == HttpClientProperties.Pool.PoolType.DISABLED) {
            return ConnectionProvider.newConnection();
        } else {
            ConnectionProvider.Builder builder = ConnectionProvider.builder(pool.getName());
            if (pool.getType() == HttpClientProperties.Pool.PoolType.FIXED) {
                // reactor remove fixed pool by fixed method from 0.9.4
                // reason: https://github.com/reactor/reactor-netty/issues/1499 and https://github.com/reactor/reactor-netty/issues/1960
                this.buildFixedConnectionPool(pool, builder);
            } else {
                // please see https://projectreactor.io/docs/netty/release/reference/index.html#_connection_pool_2
                // reactor remove elastic pool by elastic method from 0.9.4
                // reason: https://github.com/reactor/reactor-netty/issues/1499 and https://github.com/reactor/reactor-netty/issues/1960
                this.buildElasticConnectionPool(builder);
            }
            Optional.ofNullable(pool.getMaxIdleTime()).map(Duration::ofMillis).ifPresent(builder::maxIdleTime);
            Optional.ofNullable(pool.getMaxLifeTime()).map(Duration::ofMillis).ifPresent(builder::maxLifeTime);
            Optional.ofNullable(pool.getEvictionInterval()).map(Duration::ofMillis).ifPresent(builder::evictInBackground);
            builder.metrics(pool.getMetrics());
            return builder.build();
        }
    }

    /**
     * build fixed connection pool.
     *
     * @param pool    connection pool params
     * @param builder connection provider builder
     */
    public void buildFixedConnectionPool(final Pool pool,
                                         final Builder builder) {
        if (pool.getMaxConnections() <= 0) {
            throw new IllegalArgumentException("Max Connections value must be strictly positive");
        }
        if (pool.getAcquireTimeout() < 0) {
            throw new IllegalArgumentException("Acquire Timeout value must be positive");
        }
        builder.maxConnections(pool.getMaxConnections())
                .pendingAcquireTimeout(Duration.ofMillis(pool.getAcquireTimeout()))
                .pendingAcquireMaxCount(-1);
    }

    /**
     * build elastic connection provider pool.
     *
     * @param builder connection provider builder
     */
    public void buildElasticConnectionPool(final Builder builder) {
        // about the args, please see https://projectreactor.io/docs/netty/release/reference/index.html#_connection_pool_2
        builder.maxConnections(Integer.MAX_VALUE)
                .pendingAcquireTimeout(Duration.ofMillis(0))
                .pendingAcquireMaxCount(-1);
    }

}
