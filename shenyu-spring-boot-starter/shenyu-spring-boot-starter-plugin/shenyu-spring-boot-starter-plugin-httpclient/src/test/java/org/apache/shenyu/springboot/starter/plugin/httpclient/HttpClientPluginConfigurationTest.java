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

import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties.Pool.PoolType;
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
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                HttpClientPluginConfiguration.class,
                HttpClientPluginConfigurationTest.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "shenyu.httpclient.connectTimeout=3",
                "shenyu.httpclient.responseTimeout=0",
                "shenyu.httpclient.pool.PoolType=0",
                "shenyu.httpclient.pool.name=proxy",
                "shenyu.httpclient.pool.maxConnections=1",
                "shenyu.httpclient.pool.acquireTimeout=45000",
                "shenyu.httpclient.proxy.host=http://localhost",
                "shenyu.httpclient.proxy.port=18848",
                "shenyu.httpclient.proxy.username=itmiwang",
                "shenyu.httpclient.proxy.password=itmiwang",
                "shenyu.httpclient.proxy.nonProxyHostsPattern=itmiwang",
                "shenyu.httpclient.ssl.X509Certificate[]=[]",
                "shenyu.httpclient.ssl.handshakeTimeout=10000",
                "shenyu.httpclient.ssl.closeNotifyFlushTimeout=3000",
                "shenyu.httpclient.ssl.closeNotifyReadTimeout=0",
                "shenyu.httpclient.ssl.SslProvider.DefaultConfigurationType=1"
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
