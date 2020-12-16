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

package org.dromara.soul.metrics.prometheus.impl.collector.support;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.remote.JMXConnectorServer;
import javax.management.remote.JMXConnectorServerFactory;
import javax.management.remote.JMXServiceURL;
import java.io.Closeable;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.MalformedURLException;
import java.rmi.NoSuchObjectException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Map;

/**
 * Mock Jmx Server.
 *
 * @author David Liu
 */
public class MockJmxServer implements Closeable {
    private static final String RMI_SERVER_HOST_NAME_PROPERTY = "java.rmi.server.hostname";
    
    private final int registryPort;
    
    private Registry rmiRegistry;
    
    private int serverPort;
    
    private JMXConnectorServer connector;
    
    private MBeanServer mbeanServer;
    
    private boolean serverHostNamePropertySet;
    
    private String serviceUrl;
    
    /**
     * Create a JMX server running on a particular registry-port.
     *
     * @param registryPort The "RMI registry port" that connect to the server.
     */
    public MockJmxServer(final int registryPort) {
        this.registryPort = registryPort;
    }
    
    /**
     * Start our JMX service. The port must have already been called either in the {@link #MockJmxServer(int)} constructor.
     *
     * @throws JMException If the registry port has not already been set.
     */
    public synchronized void start() throws JMException {
        if (mbeanServer != null) {
            // if we've already assigned a mbean-server then there's nothing to start
            return;
        }
        if (registryPort == 0) {
            throw new IllegalStateException("registry-port must be already set when JmxServer is initialized");
        }
        startRmiRegistry();
        startJmxService();
    }
    
    /**
     * register object to jmx.
     *
     * @param dynamicMBean dynamicMBean
     * @param objectName   object name
     * @throws JMException JMException
     */
    public void register(final Object dynamicMBean, final ObjectName objectName) throws JMException {
        try {
            mbeanServer.registerMBean(dynamicMBean, objectName);
        } catch (Exception e) {
            throw createJmException("Registering JMX object " + objectName + " failed", e);
        }
    }
    
    /**
     * Same as {@link #stopThrow()} but this ignores any exceptions.
     */
    public synchronized void stop() {
        try {
            stopThrow();
        } catch (JMException e) {
            // ignored
        }
    }
    
    @Override
    public void close() throws IOException {
        try {
            stopThrow();
        } catch (JMException jme) {
            throw new IOException(jme);
        }
    }
    
    /**
     * Stop the JMX server by closing the connector and unpublishing it from the RMI registry. This throws a JMException on any issues.
     *
     * @throws JMException JMException
     */
    public synchronized void stopThrow() throws JMException {
        if (connector != null) {
            try {
                connector.stop();
            } catch (IOException e) {
                throw createJmException("Could not stop our Jmx connector server", e);
            } finally {
                connector = null;
            }
        }
        if (rmiRegistry != null) {
            try {
                UnicastRemoteObject.unexportObject(rmiRegistry, true);
            } catch (NoSuchObjectException e) {
                throw createJmException("Could not unexport our RMI registry", e);
            } finally {
                rmiRegistry = null;
            }
        }
        if (serverHostNamePropertySet) {
            System.clearProperty(RMI_SERVER_HOST_NAME_PROPERTY);
            serverHostNamePropertySet = false;
        }
    }
    
    private void startRmiRegistry() throws JMException {
        if (rmiRegistry != null) {
            return;
        }
        try {
            rmiRegistry = LocateRegistry.createRegistry(registryPort);
        } catch (IOException e) {
            throw createJmException("Unable to create RMI registry on port " + registryPort, e);
        }
    }
    
    private void startJmxService() throws JMException {
        if (connector != null) {
            return;
        }
        if (serverPort == 0) {
            serverPort = registryPort;
        }
        final String serverHost = "127.0.0.1";
        final String registryHost = "127.0.0.1";
        if (serviceUrl == null) {
            serviceUrl = "service:jmx:rmi://" + serverHost + ":" + serverPort + "/jndi/rmi://" + registryHost + ":" + registryPort + "/jmxrmi";
        }
        JMXServiceURL url;
        try {
            url = new JMXServiceURL(serviceUrl);
        } catch (MalformedURLException e) {
            throw createJmException("Malformed service url created " + serviceUrl, e);
        }
        
        Map<String, Object> envMap = null;
        
        try {
            mbeanServer = ManagementFactory.getPlatformMBeanServer();
            connector = JMXConnectorServerFactory.newJMXConnectorServer(url, envMap, mbeanServer);
        } catch (IOException e) {
            throw createJmException("Could not make our Jmx connector server on URL: " + url, e);
        }
        try {
            connector.start();
        } catch (IOException e) {
            connector = null;
            throw createJmException("Could not start our Jmx connector server on URL: " + url, e);
        }
    }
    
    /**
     * build JMException.
     *
     * @param message exception message
     * @param e       exception detail
     * @return exception
     */
    private JMException createJmException(final String message, final Exception e) {
        JMException jmException = new JMException(message);
        jmException.initCause(e);
        return jmException;
    }
}
