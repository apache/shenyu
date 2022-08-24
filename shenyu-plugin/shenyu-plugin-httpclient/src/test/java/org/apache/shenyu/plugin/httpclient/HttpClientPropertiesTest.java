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

package org.apache.shenyu.plugin.httpclient;

import io.netty.handler.ssl.SslProvider;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.util.ResourceUtils;
import reactor.netty.resources.ConnectionProvider;

import java.io.IOException;
import java.net.URL;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

/**
 * HttpClientPropertiesTest.
 */
public class HttpClientPropertiesTest {

    @Test
    public void getTrustedX509CertificatesForTrustManagerTest() {
        HttpClientProperties httpClientProperties = new HttpClientProperties();
        final HttpClientProperties.Ssl httpClientPropertiesSsl = httpClientProperties.getSsl();
        Assertions.assertNotNull(httpClientPropertiesSsl.getTrustedX509CertificatesForTrustManager());
        try (MockedStatic<CertificateFactory> factoryMockedStatic = mockStatic(CertificateFactory.class);
             MockedStatic<ResourceUtils> resourceUtilsMockedStatic = mockStatic(ResourceUtils.class)) {
            httpClientPropertiesSsl.setTrustedX509Certificates(Collections.singletonList("testPath"));
            final CertificateFactory certificateFactory = mock(CertificateFactory.class);
            factoryMockedStatic.when(() -> CertificateFactory.getInstance("X.509")).thenReturn(certificateFactory);
            final URL mockURL = mock(URL.class);
            resourceUtilsMockedStatic.when(() -> ResourceUtils.getURL(anyString())).thenReturn(mockURL);
            Assertions.assertNotNull(httpClientPropertiesSsl.getTrustedX509CertificatesForTrustManager());

            doThrow(IOException.class).when(mockURL).openStream();
            Assertions.assertThrows(ShenyuException.class, httpClientPropertiesSsl::getTrustedX509CertificatesForTrustManager);

            factoryMockedStatic.when(() -> CertificateFactory.getInstance("X.509")).thenThrow(CertificateException.class);
            Assertions.assertThrows(ShenyuException.class, httpClientPropertiesSsl::getTrustedX509CertificatesForTrustManager);

        } catch (IOException e) {
            throw new ShenyuException(e);
        }
    }

    @Test
    public void httpClientPropertiesSslTest() {
        HttpClientProperties httpClientProperties = new HttpClientProperties();
        HttpClientProperties.Ssl ssl = httpClientProperties.getSsl();
        ssl.setCloseNotifyFlushTimeout(Duration.ofMillis(3000));
        ssl.setCloseNotifyReadTimeout(Duration.ZERO);
        ssl.setDefaultConfigurationType(SslProvider.JDK);
        ssl.setHandshakeTimeout(Duration.ofMillis(10000));
        ssl.setKeyPassword("keyPassword");
        ssl.setKeyStorePath("keyStorePath");
        ssl.setKeyStoreProvider("keyStoreProvider");
        ssl.setKeyStoreType("PKCS12");
        ssl.setKeyStorePassword("keyStorePassword");
        ssl.setUseInsecureTrustManager(true);
        final ArrayList<String> arrayList = new ArrayList<>();
        ssl.setTrustedX509Certificates(arrayList);
        httpClientProperties.setSsl(ssl);

        Assertions.assertEquals(httpClientProperties.getSsl().getCloseNotifyFlushTimeout(), Duration.ofMillis(3000));
        Assertions.assertEquals(httpClientProperties.getSsl().getCloseNotifyReadTimeout(), Duration.ZERO);
        Assertions.assertEquals(httpClientProperties.getSsl().getDefaultConfigurationType(), SslProvider.JDK);
        Assertions.assertEquals(httpClientProperties.getSsl().getHandshakeTimeout(), Duration.ofMillis(10000));
        Assertions.assertEquals(httpClientProperties.getSsl().getKeyPassword(), "keyPassword");
        Assertions.assertEquals(httpClientProperties.getSsl().getKeyStorePath(), "keyStorePath");
        Assertions.assertEquals(httpClientProperties.getSsl().getKeyStoreProvider(), "keyStoreProvider");
        Assertions.assertEquals(httpClientProperties.getSsl().getKeyStoreType(), "PKCS12");
        Assertions.assertEquals(httpClientProperties.getSsl().getKeyStorePassword(), "keyStorePassword");
        Assertions.assertTrue(httpClientProperties.getSsl().isUseInsecureTrustManager());
        Assertions.assertEquals(httpClientProperties.getSsl().getTrustedX509Certificates(), arrayList);
        Assertions.assertNotNull(httpClientProperties.getSsl().getTrustedX509CertificatesForTrustManager());
        Assertions.assertThrows(ShenyuException.class, () -> httpClientProperties.getSsl().getKeyManagerFactory());
    }

    @Test
    public void httpClientPropertiesProxyTest() {
        HttpClientProperties httpClientProperties = new HttpClientProperties();
        HttpClientProperties.Proxy proxy = httpClientProperties.getProxy();
        proxy.setHost("host");
        proxy.setNonProxyHostsPattern("nonProxyHostsPattern");
        proxy.setPassword("password");
        proxy.setUsername("username");
        proxy.setPort(9095);
        httpClientProperties.setProxy(proxy);
        Assertions.assertEquals(httpClientProperties.getProxy().getHost(), "host");
        Assertions.assertEquals(httpClientProperties.getProxy().getNonProxyHostsPattern(), "nonProxyHostsPattern");
        Assertions.assertEquals(httpClientProperties.getProxy().getPassword(), "password");
        Assertions.assertEquals(httpClientProperties.getProxy().getUsername(), "username");
        Assertions.assertEquals(httpClientProperties.getProxy().getPort(), 9095);
    }

    @Test
    public void httpClientPropertiesPoolTest() {
        HttpClientProperties httpClientProperties = new HttpClientProperties();
        HttpClientProperties.Pool pool = httpClientProperties.getPool();
        pool.setAcquireTimeout(ConnectionProvider.DEFAULT_POOL_ACQUIRE_TIMEOUT);
        pool.setMaxConnections(ConnectionProvider.DEFAULT_POOL_MAX_CONNECTIONS);
        pool.setName("name");
        pool.setType(HttpClientProperties.Pool.PoolType.ELASTIC);
        pool.setMaxIdleTime(0L);
        httpClientProperties.setPool(pool);
        Assertions.assertEquals(httpClientProperties.getPool().getAcquireTimeout(), ConnectionProvider.DEFAULT_POOL_ACQUIRE_TIMEOUT);
        Assertions.assertEquals(httpClientProperties.getPool().getMaxConnections(), ConnectionProvider.DEFAULT_POOL_MAX_CONNECTIONS);
        Assertions.assertEquals(httpClientProperties.getPool().getName(), "name");
        Assertions.assertEquals(httpClientProperties.getPool().getType(), HttpClientProperties.Pool.PoolType.ELASTIC);
        Assertions.assertEquals(httpClientProperties.getPool().getMaxIdleTime(), Duration.ofMillis(0L));
    }

    @Test
    public void httpClientPropertiesThreadPoolTest() {
        HttpClientProperties httpClientProperties = new HttpClientProperties();
        HttpClientProperties.ThreadPool threadPool = httpClientProperties.getThreadPool();
        threadPool.setDaemon(true);
        threadPool.setPrefix("prefix-");
        threadPool.setSelectCount(1);
        threadPool.setWorkerCount(10);
        httpClientProperties.setThreadPool(threadPool);
        Assertions.assertEquals(httpClientProperties.getThreadPool().getDaemon(), true);
        Assertions.assertEquals(httpClientProperties.getThreadPool().getPrefix(), "prefix-");
        Assertions.assertEquals(httpClientProperties.getThreadPool().getSelectCount(), 1);
        Assertions.assertEquals(httpClientProperties.getThreadPool().getWorkerCount(), 10);
    }

    @Test
    public void httpClientPropertiesTest() {
        HttpClientProperties httpClientProperties = new HttpClientProperties();
        httpClientProperties.setAllIdleTime(1);
        httpClientProperties.setConnectTimeout(1);
        httpClientProperties.setKeepAlive(true);
        httpClientProperties.setMaxInMemorySize(1);
        httpClientProperties.setReaderIdleTime(1);
        httpClientProperties.setReadTimeout(1);
        httpClientProperties.setResponseTimeout(1L);
        httpClientProperties.setStrategy("strategy");
        httpClientProperties.setWiretap(true);
        httpClientProperties.setWriterIdleTime(1);
        httpClientProperties.setWriteTimeout(1);
        Assertions.assertEquals(httpClientProperties.getAllIdleTime(), 1);
        Assertions.assertEquals(httpClientProperties.getConnectTimeout(), 1);
        Assertions.assertTrue(httpClientProperties.isKeepAlive());
        Assertions.assertEquals(httpClientProperties.getMaxInMemorySize(), 1);
        Assertions.assertEquals(httpClientProperties.getReaderIdleTime(), 1);
        Assertions.assertEquals(httpClientProperties.getReadTimeout(), 1);
        Assertions.assertEquals(httpClientProperties.getResponseTimeout(), Duration.ofMillis(1L));
        Assertions.assertEquals(httpClientProperties.getStrategy(), "strategy");
        Assertions.assertTrue(httpClientProperties.isWiretap());
        Assertions.assertEquals(httpClientProperties.getWriterIdleTime(), 1);
        Assertions.assertEquals(httpClientProperties.getWriteTimeout(), 1);
    }
}
