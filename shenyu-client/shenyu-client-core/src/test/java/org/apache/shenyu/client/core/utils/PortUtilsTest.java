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

package org.apache.shenyu.client.core.utils;

import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.boot.web.embedded.tomcat.TomcatReactiveWebServerFactory;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.reactive.context.ReactiveWebServerApplicationContext;
import org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.http.server.reactive.HttpHandler;
import reactor.core.publisher.Mono;

import java.net.ServerSocket;

/**
 * Test for {@link PortUtils}.
 */
public class PortUtilsTest {

    @Test
    public void testFindPort() {
        DefaultListableBeanFactory beanFactory = new DefaultListableBeanFactory();
        GenericBeanDefinition beanDefinition = new GenericBeanDefinition();
        beanDefinition.setBeanClass(TomcatServletWebServerFactory.class);
        beanFactory.registerBeanDefinition("webServerFactory", beanDefinition);

        int port = PortUtils.findPort(beanFactory);
        Assertions.assertEquals(8080, port);
    }

    @Test
    public void testFindPortReportsActuallyBoundPort() {
        //issue #8426: with server.port=0 the configured value stays 0 while the
        //embedded server binds a random port; discovery must report the bound one
        final ServletWebServerApplicationContext context = new ServletWebServerApplicationContext();
        final TomcatServletWebServerFactory factory = new TomcatServletWebServerFactory(0);
        context.registerBean("servletWebServerFactory", TomcatServletWebServerFactory.class, () -> factory);
        try {
            context.refresh();
            final int bound = context.getWebServer().getPort();
            final int detected = PortUtils.findPort(context);
            Assertions.assertTrue(bound > 0);
            Assertions.assertEquals(bound, detected);
        } finally {
            context.close();
        }
    }

    @Test
    public void testFindPortReturnsConfiguredPortOfRunningServer() throws Exception {
        final int staticPort;
        try (ServerSocket socket = new ServerSocket(0)) {
            staticPort = socket.getLocalPort();
        }
        final ServletWebServerApplicationContext context = new ServletWebServerApplicationContext();
        final TomcatServletWebServerFactory factory = new TomcatServletWebServerFactory(staticPort);
        context.registerBean("servletWebServerFactory", TomcatServletWebServerFactory.class, () -> factory);
        try {
            context.refresh();
            Assertions.assertEquals(staticPort, PortUtils.findPort(context));
        } finally {
            context.close();
        }
    }

    @Test
    public void testFindPortWalksToOwningServerContext() {
        final ServletWebServerApplicationContext root = new ServletWebServerApplicationContext();
        final TomcatServletWebServerFactory factory = new TomcatServletWebServerFactory(0);
        root.registerBean("servletWebServerFactory", TomcatServletWebServerFactory.class, () -> factory);
        try {
            root.refresh();
            final AnnotationConfigApplicationContext child = new AnnotationConfigApplicationContext();
            child.setParent(root);
            try {
                child.refresh();
                final int bound = root.getWebServer().getPort();
                Assertions.assertTrue(bound > 0);
                Assertions.assertEquals(bound, PortUtils.findPort(child));
            } finally {
                child.close();
            }
        } finally {
            root.close();
        }
    }

    @Test
    public void testFindPortFromRunningReactiveServer() {
        final ReactiveWebServerApplicationContext context = new ReactiveWebServerApplicationContext();
        final TomcatReactiveWebServerFactory factory = new TomcatReactiveWebServerFactory(0);
        context.registerBean("reactiveWebServerFactory", TomcatReactiveWebServerFactory.class, () -> factory);
        context.registerBean("httpHandler", HttpHandler.class, () -> (request, response) -> Mono.empty());
        try {
            context.refresh();
            final int bound = context.getWebServer().getPort();
            final int detected = PortUtils.findPort(context);
            Assertions.assertTrue(bound > 0);
            Assertions.assertEquals(bound, detected);
        } finally {
            context.close();
        }
    }

    @Test
    public void testFindPortFailsLoudlyInsteadOfRegisteringZero() {
        //a factory configured for a random port but no running owned server
        //(context is not a web-server context): discovery must not silently
        //resolve the non-positive configured value (issue #8426 acceptance)
        try (AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext()) {
            context.registerBean("servletWebServerFactory", TomcatServletWebServerFactory.class,
                    () -> new TomcatServletWebServerFactory(0));
            context.refresh();
            Assertions.assertThrows(ShenyuException.class, () -> PortUtils.findPort(context));
        }
    }

    @Test
    public void testFindPortDiagnosesTotalDiscoveryFailure() {
        //no embedded server, no factory bean, no external connector: the
        //failure must carry the advertised-port diagnostic, not the bare
        //legacy message (issue #8426 "clearly diagnose" acceptance)
        try (AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext()) {
            context.refresh();
            final ShenyuException e = Assertions.assertThrows(ShenyuException.class,
                    () -> PortUtils.findPort(context));
            Assertions.assertTrue(e.getMessage().contains("advertised port"),
                    "unexpected diagnostic: " + e.getMessage());
        }
    }

}
