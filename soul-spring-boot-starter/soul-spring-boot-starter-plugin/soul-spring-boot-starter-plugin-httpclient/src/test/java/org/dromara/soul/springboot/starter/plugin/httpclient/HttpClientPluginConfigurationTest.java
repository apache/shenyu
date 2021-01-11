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

import org.dromara.soul.plugin.httpclient.config.HttpClientProperties;
import org.dromara.soul.plugin.httpclient.config.HttpClientProperties.Pool.PoolType;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import reactor.netty.http.client.HttpClient;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.tcp.SslProvider;
import java.time.Duration;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

/**
 * Test case for {@link HttpClientPluginConfiguration}.
 *
 * @author itmiwang
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                HttpClientPluginConfiguration.class,
                HttpClientPluginConfigurationTest.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "soul.httpclient.connectTimeout=3",
                "soul.httpclient.responseTimeout=0",
                "soul.httpclient.pool.PoolType=0",
                "soul.httpclient.pool.name=proxy",
                "soul.httpclient.pool.maxConnections=1",
                "soul.httpclient.pool.acquireTimeout=45000",
                "soul.httpclient.proxy.host=http://localhost",
                "soul.httpclient.proxy.port=18848",
                "soul.httpclient.proxy.username=itmiwang",
                "soul.httpclient.proxy.password=itmiwang",
                "soul.httpclient.proxy.nonProxyHostsPattern=itmiwang",
                "soul.httpclient.ssl.X509Certificate[]=[]",
                "soul.httpclient.ssl.handshakeTimeout=10000",
                "soul.httpclient.ssl.closeNotifyFlushTimeout=3000",
                "soul.httpclient.ssl.closeNotifyReadTimeout=0",
                "soul.httpclient.ssl.SslProvider.DefaultConfigurationType=1"
        })
@EnableAutoConfiguration
public final class HttpClientPluginConfigurationTest {
    
    @Autowired
    private HttpClient httpClient;
    
    @Autowired
    private HttpClientProperties httpClientProperties;
    
    @Test
    public void testHttpClientProperties() {
        assertThat(httpClientProperties.getConnectTimeout(), is(3));
        assertThat(httpClientProperties.getResponseTimeout(), is(Duration.ZERO));
        assertThat(httpClientProperties.getPool().getType(), is(PoolType.ELASTIC));
        assertThat(httpClientProperties.getPool().getName(), is("proxy"));
        assertThat(httpClientProperties.getPool().getMaxConnections(), is(1));
        assertThat(httpClientProperties.getPool().getAcquireTimeout(), is(ConnectionProvider.DEFAULT_POOL_ACQUIRE_TIMEOUT));
        assertThat(httpClientProperties.getProxy().getHost(), is("http://localhost"));
        assertThat(httpClientProperties.getProxy().getPort(), is(18848));
        assertThat(httpClientProperties.getProxy().getUsername(), is("itmiwang"));
        assertThat(httpClientProperties.getProxy().getPassword(), is("itmiwang"));
        assertThat(httpClientProperties.getProxy().getNonProxyHostsPattern(), is("itmiwang"));
        assertThat(httpClientProperties.getSsl().getHandshakeTimeout(), is(Duration.ofMillis(10000)));
        assertNotNull(httpClientProperties.getSsl().getTrustedX509Certificates());
        assertThat(httpClientProperties.getSsl().getCloseNotifyFlushTimeout(), is(Duration.ofMillis(3000)));
        assertThat(httpClientProperties.getSsl().getCloseNotifyReadTimeout(), is(Duration.ZERO));
        assertThat(httpClientProperties.getSsl().getDefaultConfigurationType(), is(SslProvider.DefaultConfigurationType.TCP));
    }
    
    @Test
    public void testHttpClient() {
        assertNotNull(httpClient);
    }
}
