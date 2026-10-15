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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.time.Duration;

import io.micrometer.core.instrument.Metrics;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Answers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import reactor.netty.http.client.HttpClient;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.resources.ConnectionProvider.Builder;

/**
 * Test case for {@link HttpClientPluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class HttpClientPluginConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(HttpClientPluginConfiguration.class))
                .withBean(HttpClientPluginConfigurationTest.class)
                .withBean(ServerProperties.class);
    }

    @Test
    public void testHttpClientProperties() {
        applicationContextRunner
                .withPropertyValues(
                        "debug=true",
                        "shenyu.httpclient.connectTimeout=3",
                        "shenyu.httpclient.responseTimeout=0",
                        "shenyu.httpclient.pool.name=proxy",
                        "shenyu.httpclient.pool.maxConnections=1",
                        "shenyu.httpclient.pool.acquireTimeout=45000",
                        "shenyu.httpclient.pool.pending-acquire-max-count=7",
                        "shenyu.httpclient.proxy.host=http://localhost",
                        "shenyu.httpclient.proxy.port=18848",
                        "shenyu.httpclient.proxy.username=itmiwang",
                        "shenyu.httpclient.proxy.password=itmiwang",
                        "shenyu.httpclient.proxy.nonProxyHostsPattern=itmiwang",
                        "shenyu.httpclient.ssl.X509Certificate[]=[]",
                        "shenyu.httpclient.ssl.handshakeTimeout=10000",
                        "shenyu.httpclient.ssl.closeNotifyFlushTimeout=3000",
                        "shenyu.httpclient.ssl.closeNotifyReadTimeout=0"
                )
                .run(context -> {
                    HttpClientProperties properties = context.getBean("httpClientProperties", HttpClientProperties.class);
                    assertNotNull(properties);
                    assertThat(properties.getConnectTimeout(), is(3));
                    assertThat(properties.getResponseTimeout(), is(Duration.ZERO));
                    assertThat(properties.getPool().getType(), is(HttpClientProperties.Pool.PoolType.FIXED));
                    assertThat(properties.getPool().getName(), is("proxy"));
                    assertThat(properties.getPool().getMaxConnections(), is(1));
                    assertThat(properties.getPool().getAcquireTimeout(), is(ConnectionProvider.DEFAULT_POOL_ACQUIRE_TIMEOUT));
                    assertThat(properties.getPool().getPendingAcquireMaxCount(), is(7));
                    assertThat(properties.getProxy().getHost(), is("http://localhost"));
                    assertThat(properties.getProxy().getPort(), is(18848));
                    assertThat(properties.getProxy().getUsername(), is("itmiwang"));
                    assertThat(properties.getProxy().getPassword(), is("itmiwang"));
                    assertThat(properties.getProxy().getNonProxyHostsPattern(), is("itmiwang"));
                    assertThat(properties.getSsl().getHandshakeTimeout(), is(Duration.ofMillis(10000)));
                    assertNotNull(properties.getSsl().getTrustedX509Certificates());
                    assertThat(properties.getSsl().getCloseNotifyFlushTimeout(), is(Duration.ofMillis(3000)));
                    assertThat(properties.getSsl().getCloseNotifyReadTimeout(), is(Duration.ZERO));
                });
    }

    @Test
    public void testFixedConnectionPoolUsesBoundedPendingQueue() {
        HttpClientProperties.Pool pool = new HttpClientProperties.Pool();
        Builder builder = mock(Builder.class, Answers.RETURNS_SELF);
        HttpClientFactory factory = new HttpClientFactory(new HttpClientProperties(), null, new ServerProperties());

        factory.buildFixedConnectionPool(pool, builder);

        verify(builder).maxConnections(ConnectionProvider.DEFAULT_POOL_MAX_CONNECTIONS);
        verify(builder).pendingAcquireTimeout(Duration.ofMillis(3000L));
        verify(builder).pendingAcquireMaxCount(2 * ConnectionProvider.DEFAULT_POOL_MAX_CONNECTIONS);
    }

    @Test
    void explicitQueueLimitAndLegacyUnboundedOptIn() {
        HttpClientProperties properties = new HttpClientProperties();
        HttpClientFactory factory = new HttpClientFactory(properties, null, new ServerProperties());
        Builder builder = mock(Builder.class, Answers.RETURNS_SELF);
        properties.getPool().setPendingAcquireMaxCount(3);
        factory.buildFixedConnectionPool(properties.getPool(), builder);
        verify(builder).pendingAcquireMaxCount(3);
        properties.getPool().setPendingAcquireMaxCount(-1);
        factory.buildElasticConnectionPool(builder);
        verify(builder).pendingAcquireMaxCount(-1);
    }

    @ParameterizedTest
    @ValueSource(ints = {0, -2, Integer.MIN_VALUE})
    void invalidQueueCountsAreRejected(final int count) {
        HttpClientProperties properties = new HttpClientProperties();
        properties.getPool().setPendingAcquireMaxCount(count);
        HttpClientFactory factory = new HttpClientFactory(properties, null, new ServerProperties());
        assertThrows(IllegalArgumentException.class, () -> factory.buildFixedConnectionPool(properties.getPool(), ConnectionProvider.builder("test")));
        assertThrows(IllegalArgumentException.class, () -> factory.buildElasticConnectionPool(ConnectionProvider.builder("test")));
    }

    @Test
    void derivedQueueLimitDoesNotOverflow() {
        HttpClientProperties properties = new HttpClientProperties();
        properties.getPool().setMaxConnections(Integer.MAX_VALUE);
        Builder builder = mock(Builder.class, Answers.RETURNS_SELF);
        new HttpClientFactory(properties, null, new ServerProperties()).buildFixedConnectionPool(properties.getPool(), builder);
        verify(builder).pendingAcquireMaxCount(Integer.MAX_VALUE);
    }

    @Test
    void metricsAreOptInAndDisabledPoolStillBypassesLimits() {
        HttpClientProperties properties = new HttpClientProperties();
        properties.getPool().setName("metrics-disabled");
        HttpClientFactory factory = new HttpClientFactory(properties, null, new ServerProperties());
        HttpClient client = factory.createInstance();
        assertThat(Metrics.globalRegistry.find("shenyu.httpclient.pool.acquire").tag("name", "metrics-disabled").timers().isEmpty(), is(true));
        client.configuration().connectionProvider().dispose();
        properties.getPool().setType(HttpClientProperties.Pool.PoolType.DISABLED);
        properties.getPool().setMaxConnections(0);
        properties.getPool().setPendingAcquireMaxCount(0);
        assertNotNull(factory.createInstance());
    }

    @Test
    public void testHttpClient() {
        applicationContextRunner
                .withPropertyValues("debug=true")
                .run(context -> {
                    HttpClient client = context.getBean("httpClient", HttpClient.class);
                    assertNotNull(client);
                });
    }

    @Test
    public void testWebClientPlugin() {
        applicationContextRunner
                .withPropertyValues(
                        "debug=true",
                        "shenyu.httpclient.strategy=webClient"
                )
                .run(context -> {
                    ShenyuPlugin plugin = context.getBean("webClientPlugin", ShenyuPlugin.class);
                    assertNotNull(plugin);
                });
    }

    @Test
    public void testNettyHttpClientPlugin() {
        applicationContextRunner
                .withPropertyValues(
                        "debug=true",
                        "shenyu.httpclient.strategy=netty"
                )
                .run(context -> {
                    ShenyuPlugin plugin = context.getBean("nettyHttpClientPlugin", ShenyuPlugin.class);
                    assertNotNull(plugin);
                });
    }
}
