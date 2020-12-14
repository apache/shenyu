package org.dromara.soul.metrics.prometheus.impl.collector.support;

import javax.management.DynamicMBean;
import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.remote.JMXConnectorServer;
import javax.management.remote.JMXConnectorServerFactory;
import javax.management.remote.JMXServiceURL;
import javax.management.remote.rmi.RMIConnectorServer;
import java.io.Closeable;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.ServerSocket;
import java.rmi.NoSuchObjectException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.RMIServerSocketFactory;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Map;

/**
 * JmxTestServer.
 *
 * @author David Liu
 */
public class JmxTestServer implements Closeable {
    private static final String RMI_SERVER_HOST_NAME_PROPERTY = "java.rmi.server.hostname";
    
    private Registry rmiRegistry;
    
    private InetAddress inetAddress;
    
    private int serverPort;
    
    private int registryPort;
    
    private JMXConnectorServer connector;
    
    private MBeanServer mbeanServer;
    
    private int registeredCount;
    
    private RMIServerSocketFactory serverSocketFactory;
    
    private boolean serverHostNamePropertySet;
    
    private String serviceUrl;
    
    /**
     * Create a JMX server running on a particular registry-port.
     *
     * @param registryPort The "RMI registry port" that you specify in jconsole to connect to the server. See
     *                     {@link #setRegistryPort(int)}.
     */
    public JmxTestServer(final int registryPort) {
        this.registryPort = registryPort;
    }
    
    /**
     * Create a JMX server running on a particular address and registry-port.
     *
     * @param inetAddress  Address to bind to. If you use on the non-address constructors, it will bind to all interfaces.
     * @param registryPort The "RMI registry port" that you specify in jconsole to connect to the server. See
     *                     {@link #setRegistryPort(int)}.
     */
    public JmxTestServer(final InetAddress inetAddress, final int registryPort) {
        this.inetAddress = inetAddress;
        this.registryPort = registryPort;
    }
    
    /**
     * If you pass in true, this will create a JmxServer that uses the existing JVM platform's MBeanServer. This calls
     * through to the {@link ManagementFactory#getPlatformMBeanServer()} which will create one if it doesn't already
     * exist.
     */
    public JmxTestServer(final boolean usePlatformMBeanServer) {
        if (usePlatformMBeanServer) {
            this.mbeanServer = ManagementFactory.getPlatformMBeanServer();
        }
    }
    
    /**
     * Start our JMX service. The port must have already been called either in the {@link #JmxTestServer(int)} constructor
     * or the {@link #setRegistryPort(int)} method before this is called.
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
    public void register(final DynamicMBean dynamicMBean, final ObjectName objectName) throws JMException {
        try {
            mbeanServer.registerMBean(dynamicMBean, objectName);
            registeredCount++;
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
    
    /**
     * Not required. Default is to bind to local interfaces.
     *
     * @param inetAddress net address
     */
    public void setInetAddress(final InetAddress inetAddress) {
        this.inetAddress = inetAddress;
    }
    
    /**
     * This is actually calls {@link #setRegistryPort(int)}.
     *
     * @param port port
     */
    public void setPort(final int port) {
        setRegistryPort(port);
    }
    
    /**
     * get registry port.
     *
     * @return registry port
     */
    public int getRegistryPort() {
        return registryPort;
    }
    
    /**
     * set registry port.
     *
     * @param registryPort registry port
     */
    public void setRegistryPort(final int registryPort) {
        this.registryPort = registryPort;
    }
    
    /**
     * Number of registered objects.
     *
     * @return registered mbean count
     */
    public int getRegisteredCount() {
        return registeredCount;
    }
    
    private void startRmiRegistry() throws JMException {
        if (rmiRegistry != null) {
            return;
        }
        try {
            if (inetAddress == null) {
                rmiRegistry = LocateRegistry.createRegistry(registryPort);
            } else {
                if (serverSocketFactory == null) {
                    serverSocketFactory = new LocalSocketFactory(inetAddress);
                }
                if (System.getProperty(RMI_SERVER_HOST_NAME_PROPERTY) == null) {
                    System.setProperty(RMI_SERVER_HOST_NAME_PROPERTY, inetAddress.getHostAddress());
                    serverHostNamePropertySet = true;
                }
                
                rmiRegistry = LocateRegistry.createRegistry(registryPort, null, serverSocketFactory);
            }
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
        String serverHost = "localhost";
        String registryHost = "";
        if (inetAddress != null) {
            String hostAddr = inetAddress.getHostAddress();
            serverHost = hostAddr;
            registryHost = hostAddr;
        }
        if (serviceUrl == null) {
            serviceUrl = "service:jmx:rmi://" + serverHost + ":" + serverPort + "/jndi/rmi://" + registryHost + ":"
                    + registryPort + "/jmxrmi";
        }
        JMXServiceURL url;
        try {
            url = new JMXServiceURL(serviceUrl);
        } catch (MalformedURLException e) {
            throw createJmException("Malformed service url created " + serviceUrl, e);
        }
        
        Map<String, Object> envMap = null;
        if (serverSocketFactory != null) {
            envMap = new HashMap<>();
            envMap.put(RMIConnectorServer.RMI_SERVER_SOCKET_FACTORY_ATTRIBUTE, serverSocketFactory);
        }
        
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
    
    static class LocalSocketFactory implements RMIServerSocketFactory {
        
        private final InetAddress inetAddress;
        
        LocalSocketFactory(final InetAddress inetAddress) {
            this.inetAddress = inetAddress;
        }
        
        @Override
        public ServerSocket createServerSocket(final int port) throws IOException {
            return new ServerSocket(port, 0, inetAddress);
        }
        
        @Override
        public int hashCode() {
            return this.inetAddress == null ? 0 : this.inetAddress.hashCode();
        }
        
        @Override
        public boolean equals(final Object obj) {
            if (obj == null || getClass() != obj.getClass()) {
                return false;
            }
            LocalSocketFactory other = (LocalSocketFactory) obj;
            if (this.inetAddress == null) {
                return other.inetAddress == null;
            } else {
                return this.inetAddress.equals(other.inetAddress);
            }
        }
    }
}
