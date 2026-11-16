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
import io.netty.handler.ssl.util.InsecureTrustManagerFactory;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransportProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.netty.NettyPipeline;
import reactor.netty.http.Http11SslContextSpec;
import reactor.netty.http.client.HttpClient;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.tcp.SslProvider;
import reactor.netty.transport.ProxyProvider;

import java.time.Duration;
import java.util.Objects;

/**
 * Owns the shared Reactor Netty resources used by the AI proxy transport.
 */
public final class AiProxyHttpClientResources implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyHttpClientResources.class);

    private static final Duration DISPOSAL_WAIT_MARGIN = Duration.ofSeconds(1);

    private final ConnectionProvider connectionProvider;

    private final HttpClient httpClient;

    private final Duration gracefulDisposeTimeout;

    /**
     * Create the shared AI proxy HTTP client resources.
     *
     * @param properties AI proxy transport properties
     */
    public AiProxyHttpClientResources(final AiProxyTransportProperties properties) {
        final AiProxyTransportProperties nonNullProperties = Objects.requireNonNull(properties, "properties must not be null");
        validateProxy(nonNullProperties.getProxy());
        this.gracefulDisposeTimeout = nonNullProperties.getPool().getGracefulDisposeTimeout();
        this.connectionProvider = createConnectionProvider(nonNullProperties.getPool());
        this.httpClient = createHttpClient(connectionProvider, nonNullProperties);
    }

    /**
     * Get the configured shared HTTP client.
     *
     * @return configured HTTP client
     */
    public HttpClient getHttpClient() {
        return httpClient;
    }

    private ConnectionProvider createConnectionProvider(final AiProxyTransportProperties.Pool pool) {
        if (!pool.isEnabled()) {
            return ConnectionProvider.newConnection();
        }
        return ConnectionProvider.builder(pool.getName())
                .maxConnections(pool.getMaxConnections())
                .pendingAcquireMaxCount(pool.getPendingAcquireMaxCount())
                .pendingAcquireTimeout(pool.getPendingAcquireTimeout())
                .maxIdleTime(pool.getMaxIdleTime())
                .maxLifeTime(pool.getMaxLifeTime())
                .evictInBackground(pool.getEvictionInterval())
                .disposeTimeout(pool.getGracefulDisposeTimeout())
                .build();
    }

    private HttpClient createHttpClient(final ConnectionProvider provider, final AiProxyTransportProperties properties) {
        HttpClient client = HttpClient.create(provider)
                .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, connectTimeoutMillis(properties))
                .responseTimeout(properties.getResponseHeaderTimeout())
                .doOnResponse((response, connection) ->
                        connection.removeHandler(NettyPipeline.ResponseTimeoutHandler));
        if (!properties.getTls().isVerify()) {
            LOG.warn("AI proxy upstream TLS certificate verification is disabled");
            client = configureInsecureTls(client);
        }
        if (properties.getProxy().isEnabled()) {
            client = configureProxy(client, properties.getProxy());
        }
        return client;
    }

    private int connectTimeoutMillis(final AiProxyTransportProperties properties) {
        return Math.toIntExact(properties.getConnectTimeout().toMillis());
    }

    private HttpClient configureInsecureTls(final HttpClient client) {
        final SslProvider provider = SslProvider.builder()
                .sslContext(Http11SslContextSpec.forClient()
                        .configure(builder -> builder.trustManager(InsecureTrustManagerFactory.INSTANCE)))
                .build();
        return client.secure(provider);
    }

    private HttpClient configureProxy(final HttpClient client, final AiProxyTransportProperties.Proxy proxy) {
        return client.proxy(spec -> {
            final ProxyProvider.Builder builder = spec
                    .type(ProxyProvider.Proxy.HTTP)
                    .host(proxy.getHost())
                    .port(proxy.getPort());
            if (StringUtils.isNotBlank(proxy.getUsername())) {
                builder.username(proxy.getUsername());
            }
            if (StringUtils.isNotBlank(proxy.getPassword())) {
                builder.password(ignored -> proxy.getPassword());
            }
            if (StringUtils.isNotBlank(proxy.getNonProxyHostsPattern())) {
                builder.nonProxyHosts(proxy.getNonProxyHostsPattern());
            }
        });
    }

    private void validateProxy(final AiProxyTransportProperties.Proxy proxy) {
        if (!proxy.isEnabled()) {
            return;
        }
        if (StringUtils.isBlank(proxy.getHost())) {
            throw new IllegalArgumentException("proxy host must not be blank when proxy is enabled");
        }
        if (proxy.getPort() < 1 || proxy.getPort() > 65535) {
            throw new IllegalArgumentException("proxy port must be between 1 and 65535 when proxy is enabled");
        }
        if (StringUtils.isNotBlank(proxy.getPassword()) && StringUtils.isBlank(proxy.getUsername())) {
            throw new IllegalArgumentException("proxy username must be configured when proxy password is configured");
        }
    }

    @Override
    public void close() {
        try {
            connectionProvider.disposeLater().block(gracefulDisposeTimeout.plus(DISPOSAL_WAIT_MARGIN));
        } catch (RuntimeException ex) {
            LOG.warn("Failed to gracefully dispose AI proxy connection provider", ex);
            connectionProvider.dispose();
        }
    }
}
