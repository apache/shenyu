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
import org.junit.Assert;
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
                "soul.httpclient.ssl.SslProvider.DefaultConfigurationType=1",
                "soul.httpclient.strategy.webClient="
        })
@EnableAutoConfiguration
public class HttpClientPluginConfigurationTest {
    
    @Autowired
    private HttpClient httpClient;
    
    @Autowired
    private HttpClientProperties httpClientProperties;
    
    @Test
    public void testHttpClientProperties() {
        Assert.assertSame(Integer.valueOf(3), httpClientProperties.getConnectTimeout());
        Assert.assertEquals(Duration.ZERO, httpClientProperties.getResponseTimeout());
        Assert.assertEquals(PoolType.ELASTIC, httpClientProperties.getPool().getType());
        Assert.assertEquals("proxy", httpClientProperties.getPool().getName());
        Assert.assertSame(Integer.valueOf(1), httpClientProperties.getPool().getMaxConnections());
        Assert.assertEquals(ConnectionProvider.DEFAULT_POOL_ACQUIRE_TIMEOUT, httpClientProperties.getPool().getAcquireTimeout().longValue());
        Assert.assertEquals("http://localhost", httpClientProperties.getProxy().getHost());
        Assert.assertEquals(Integer.valueOf(18848), httpClientProperties.getProxy().getPort());
        Assert.assertEquals("itmiwang", httpClientProperties.getProxy().getUsername());
        Assert.assertEquals("itmiwang", httpClientProperties.getProxy().getPassword());
        Assert.assertEquals("itmiwang", httpClientProperties.getProxy().getNonProxyHostsPattern());
        Assert.assertEquals(Duration.ofMillis(10000), httpClientProperties.getSsl().getHandshakeTimeout());
        Assert.assertNotNull(httpClientProperties.getSsl().getTrustedX509Certificates());
        Assert.assertEquals(Duration.ofMillis(3000), httpClientProperties.getSsl().getCloseNotifyFlushTimeout());
        Assert.assertEquals(Duration.ZERO, httpClientProperties.getSsl().getCloseNotifyReadTimeout());
        Assert.assertEquals(SslProvider.DefaultConfigurationType.TCP, httpClientProperties.getSsl().getDefaultConfigurationType());
    }
    
    @Test
    public void testHttpClient() {
        Assert.assertNotNull(httpClient);
    }
}
