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

package org.apache.shenyu.client.core.register;

import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.boot.web.context.WebServerApplicationContext;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServer;
import org.springframework.context.ApplicationContext;
import org.springframework.core.env.Environment;

import java.util.Properties;

/**
 * Test for {@link ClientRegisterConfigImpl} port resolution (issue #8426).
 */
public class ClientRegisterConfigImplTest {

    private static final int DISCOVERED_PORT = 27987;

    @Test
    public void configuredPositivePortIsAuthoritative() {
        final ClientRegisterConfigImpl config = new ClientRegisterConfigImpl(
                clientConfig(props("port", "18443")), RpcTypeEnum.HTTP,
                Mockito.mock(ApplicationContext.class), Mockito.mock(Environment.class));
        Assertions.assertEquals(18443, config.getPort());
    }

    @Test
    public void zeroConfiguredPortResolvesActuallyBoundServerPort() {
        //issue #8426: an explicitly configured 0 means random-port binding and
        //must not be registered verbatim; the bound port wins instead
        final ApplicationContext context = webServerContext(DISCOVERED_PORT);
        final ClientRegisterConfigImpl config = new ClientRegisterConfigImpl(
                clientConfig(props("port", "0")), RpcTypeEnum.HTTP, context, Mockito.mock(Environment.class));
        Assertions.assertEquals(DISCOVERED_PORT, config.getPort());
    }

    @Test
    public void absentPortResolvesActuallyBoundServerPort() {
        final ApplicationContext context = webServerContext(DISCOVERED_PORT);
        final ClientRegisterConfigImpl config = new ClientRegisterConfigImpl(
                clientConfig(new Properties()), RpcTypeEnum.HTTP, context, Mockito.mock(Environment.class));
        Assertions.assertEquals(DISCOVERED_PORT, config.getPort());
    }

    @Test
    public void zeroResolutionWithoutRunningServerFailsLoudly() {
        //a factory configured for a random port but no owned running server
        //(context is not a web-server context): registration must fail loudly
        //instead of silently resolving the non-positive configured value
        final ApplicationContext context = Mockito.mock(ApplicationContext.class);
        final DefaultListableBeanFactory beanFactory = new DefaultListableBeanFactory();
        beanFactory.registerBeanDefinition("servletWebServerFactory", BeanDefinitionBuilder
                .rootBeanDefinition(TomcatServletWebServerFactory.class)
                .addConstructorArgValue(0)
                .getBeanDefinition());
        Mockito.when(context.getAutowireCapableBeanFactory()).thenReturn(beanFactory);
        final ClientRegisterConfigImpl config = new ClientRegisterConfigImpl(
                clientConfig(props("port", "0")), RpcTypeEnum.HTTP, context, Mockito.mock(Environment.class));
        Assertions.assertThrows(ShenyuException.class, config::getPort);
    }

    private static ApplicationContext webServerContext(final int port) {
        final WebServer server = webServer(port);
        final WebServerApplicationContext context = Mockito.mock(WebServerApplicationContext.class);
        Mockito.when(context.getWebServer()).thenReturn(server);
        return context;
    }

    private static WebServer webServer(final int port) {
        final WebServer webServer = Mockito.mock(WebServer.class);
        Mockito.when(webServer.getPort()).thenReturn(port);
        return webServer;
    }

    private static Properties props(final String key, final String value) {
        final Properties properties = new Properties();
        properties.setProperty(key, value);
        return properties;
    }

    private static ShenyuClientConfig clientConfig(final Properties props) {
        final ShenyuClientConfig config = new ShenyuClientConfig();
        final ShenyuClientConfig.ClientPropertiesConfig clientProperties = new ShenyuClientConfig.ClientPropertiesConfig();
        clientProperties.setProps(props);
        config.getClient().put(RpcTypeEnum.HTTP.getName(), clientProperties);
        return config;
    }
}
