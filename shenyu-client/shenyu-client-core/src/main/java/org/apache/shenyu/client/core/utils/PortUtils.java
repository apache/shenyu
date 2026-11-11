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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.context.ApplicationContext;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import java.lang.management.ManagementFactory;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Objects;
import java.util.Set;

/**
 * get the port number exposed by the current tomcat.
 */
public class PortUtils {

    /**
     * Guard against malformed context parent chains when walking up to the
     * application context that owns the running web server.
     */
    private static final int MAX_CONTEXT_DEPTH = 8;

    /**
     * Referenced reflectively so this module keeps no compile-time spring-boot dependency.
     */
    private static final String WEB_SERVER_APPLICATION_CONTEXT_CLASS_NAME =
            "org.springframework.boot.web.context.WebServerApplicationContext";

    /**
     * Referenced reflectively so this module keeps no compile-time spring-boot dependency.
     */
    private static final String WEB_SERVER_CLASS_NAME =
            "org.springframework.boot.web.server.WebServer";

    /**
     * Find the port the application's own embedded web server actually bound.
     * Note: preferred over {@link #findPort(BeanFactory)} whenever an
     * {@link ApplicationContext} is at hand: the factory-reflective variant
     * reports the <em>configured</em> port, which is {@code 0} when the server
     * was started on a random port ({@code server.port=0}). This variant asks
     * the running {@code WebServer} for its bound port, walking up the context
     * parent chain so classic child contexts resolve to their owning server
     * context; a management context is never in that chain and is therefore
     * never selected. When no bound port can be observed (no embedded server,
     * external deployment), discovery falls back to the legacy chain and a
     * non-positive resolution fails loudly instead of registering an unusable
     * address.
     *
     * @param applicationContext the application context the client is registering from
     * @return the positive port the owned web server bound
     * @throws ShenyuException when no usable port can be discovered
     */
    public static int findPort(final ApplicationContext applicationContext) {
        final Integer bound = findBoundWebServerPort(applicationContext);
        if (Objects.nonNull(bound) && bound > 0) {
            return bound;
        }
        final int legacy;
        try {
            legacy = findPort(applicationContext.getAutowireCapableBeanFactory());
        } catch (ShenyuException e) {
            throw new ShenyuException("can not find port automatically: no embedded web server reported a bound port "
                    + "and no external container connector was detected; configure the advertised port explicitly, "
                    + "e.g. shenyu.client.http.props.port", e);
        }
        if (legacy <= 0) {
            throw new ShenyuException("can not find port automatically: port discovery resolved the non-positive port "
                    + legacy + "; configure the advertised port explicitly, e.g. shenyu.client.http.props.port");
        }
        return legacy;
    }

    /**
     * Note: springboot 1.x version has been made compatible.
     * Note: springmvc with external tomcat had been made compatible.
     * Note: In this way, no matter what container is actually used, you can get the port that is actually started in the end.
     * Note: see {@code org.springframework.boot.context.embedded.AbstractConfigurableEmbeddedServletContainer#getPort()}
     * for springboot 1.x;
     * Note: see {@code org.springframework.boot.web.server.AbstractConfigurableWebServerFactory#getPort()}
     * for springboot 2.x
     *
     * @param beanFactory beanFactory
     * @return port number
     * @throws ShenyuException when can not find port
     */
    @SuppressWarnings("all")
    public static int findPort(final BeanFactory beanFactory) {
        try {
            //works fine for springboot 2.x
            return getPort(beanFactory, "org.springframework.boot.web.server.AbstractConfigurableWebServerFactory");
        } catch (Exception ignored) {
        }
        try {
            //works fine for springboot 1.x
            return getPort(beanFactory, "org.springframework.boot.context.embedded.AbstractConfigurableEmbeddedServletContainer");
        } catch (Exception ignored) {
        }
        try {
            //works for springmvc with external tomcat
            return PortUtils.getPort();
        } catch (Exception e) {
            throw new ShenyuException("can not find port automatically ! ");
        }
    }

    /**
     * get container port.
     *
     * @param beanFactory beanFactory
     * @param className   className
     * @return container port
     */
    private static int getPort(final BeanFactory beanFactory, final String className) throws ClassNotFoundException,
            NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        final Class<?> clazz = Class.forName(className);
        final Method method = clazz.getMethod("getPort");
        final Object bean = beanFactory.getBean(clazz);
        return (int) method.invoke(bean);
    }

    /**
     * get the current tomcat port number.
     * Note: This method is not supported when there are multiple instances of external Tomcat.
     *
     * @return tomcat port number
     * @throws Exception when failed to get port
     */
    public static Integer getPort() throws Exception {
        MBeanServer mBeanServer = ManagementFactory.getPlatformMBeanServer();
        Set<ObjectName> objectNames = mBeanServer.queryNames(new ObjectName("*:type=Connector,*"), null);
        if (CollectionUtils.isEmpty(objectNames)) {
            throw new IllegalStateException("Cannot get the names of MBeans controlled by the MBean server.");
        }
        //This method is not supported when there are multiple instances of external Tomcat
        if (objectNames.size() > 1) {
            throw new IllegalStateException("Not supported when there are multiple instances of external Tomcat.");
        }
        ObjectName objectName = objectNames.iterator().next();
        String protocol = String.valueOf(mBeanServer.getAttribute(objectName, "protocol"));
        String port = String.valueOf(mBeanServer.getAttribute(objectName, "port"));
        // The property name is HTTP1.1, org.apache.coyote.http11.Http11NioProtocol under linux
        if ("HTTP/1.1".equals(protocol) || "org.apache.coyote.http11.Http11NioProtocol".equals(protocol)) {
            return Integer.parseInt(port);
        }
        throw new IllegalStateException("failed to get the HTTP port of the current tomcat");
    }

    /**
     * Read the port the running web server bound from the application context
     * owning it, walking up the parent chain so classic child contexts resolve
     * to their owning server context. Both spring-boot types are resolved
     * reflectively because this module has no compile-time spring-boot
     * dependency.
     *
     * @param applicationContext starting context of the walk
     * @return the bound port, or {@code null} when it cannot be observed
     */
    private static Integer findBoundWebServerPort(final ApplicationContext applicationContext) {
        final Class<?> webServerApplicationContextClass;
        try {
            webServerApplicationContextClass = Class.forName(WEB_SERVER_APPLICATION_CONTEXT_CLASS_NAME);
        } catch (ClassNotFoundException e) {
            //spring-boot web classes are not on the classpath: nothing to probe
            return null;
        }
        final Class<?> webServerClass;
        try {
            webServerClass = Class.forName(WEB_SERVER_CLASS_NAME);
        } catch (ClassNotFoundException e) {
            return null;
        }
        ApplicationContext current = applicationContext;
        int depth = 0;
        while (Objects.nonNull(current) && depth++ < MAX_CONTEXT_DEPTH) {
            if (webServerApplicationContextClass.isInstance(current)) {
                try {
                    final Object webServer = webServerApplicationContextClass
                            .getMethod("getWebServer").invoke(current);
                    if (Objects.nonNull(webServer)) {
                        return (Integer) webServerClass.getMethod("getPort").invoke(webServer);
                    }
                } catch (ReflectiveOperationException ignored) {
                    //fall through to legacy discovery in the caller
                    return null;
                }
            }
            current = current.getParent();
        }
        return null;
    }

}
