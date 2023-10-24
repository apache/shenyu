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

import javax.management.MBeanServer;
import javax.management.ObjectName;
import java.lang.management.ManagementFactory;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Set;

/**
 * get the port number exposed by the current tomcat.
 */
public class PortUtils {

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

}
