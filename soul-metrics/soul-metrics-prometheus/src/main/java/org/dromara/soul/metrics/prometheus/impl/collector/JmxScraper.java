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

package org.dromara.soul.metrics.prometheus.impl.collector;

import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.InstanceNotFoundException;
import javax.management.JMException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.ReflectionException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularType;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import javax.management.remote.rmi.RMIConnectorServer;
import javax.naming.Context;
import javax.rmi.ssl.SslRMIClientSocketFactory;

/**
 * The type Jmx scraper.
 */
public class JmxScraper {
    
    private static final Logger LOGGER = Logger.getLogger(JmxScraper.class.getName());
    
    private final MBeanReceiver receiver;
    
    private final String jmxUrl;
    
    private final String username;
    
    private final String password;
    
    private final boolean ssl;
    
    private final List<ObjectName> whitelistObjectNames;
    
    private final List<ObjectName> blacklistObjectNames;
    
    private final JmxMBeanPropertyCache jmxMBeanPropertyCache;
    
    /**
     * Instantiates a new Jmx scraper.
     *
     * @param jmxUrl                the jmx url
     * @param username              the username
     * @param password              the password
     * @param ssl                   the ssl
     * @param whitelistObjectNames  the whitelist object names
     * @param blacklistObjectNames  the blacklist object names
     * @param receiver              the receiver
     * @param jmxMBeanPropertyCache the jmx m bean property cache
     */
    public JmxScraper(final String jmxUrl, final String username, final String password, final boolean ssl,
                      final List<ObjectName> whitelistObjectNames, final List<ObjectName> blacklistObjectNames,
                      final MBeanReceiver receiver, final JmxMBeanPropertyCache jmxMBeanPropertyCache) {
        this.jmxUrl = jmxUrl;
        this.receiver = receiver;
        this.username = username;
        this.password = password;
        this.ssl = ssl;
        this.whitelistObjectNames = whitelistObjectNames;
        this.blacklistObjectNames = blacklistObjectNames;
        this.jmxMBeanPropertyCache = jmxMBeanPropertyCache;
    }
    
    private static void logScrape(final ObjectName mbeanName, final Set<String> names, final String msg) {
        logScrape(mbeanName + "_" + names, msg);
    }
    
    private static void logScrape(final ObjectName mbeanName, final MBeanAttributeInfo attr, final String msg) {
        logScrape(mbeanName + "'_'" + attr.getName(), msg);
    }
    
    private static void logScrape(final String name, final String msg) {
        LOGGER.log(Level.FINE, "scrape: '" + name + "': " + msg);
    }
    
    /**
     * Get a list of mbeans on host_port and scrape their values.
     * Values are passed to the receiver in a single thread.
     *
     * @throws IOException the exception
     */
    public void doScrape() throws IOException {
        MBeanServerConnection beanConn;
        JMXConnector jmxc = null;
        if (jmxUrl.isEmpty()) {
            beanConn = ManagementFactory.getPlatformMBeanServer();
        } else {
            Map<String, Object> environment = new HashMap<>();
            if (username != null && username.length() != 0 && password != null && password.length() != 0) {
                String[] credent = new String[]{username, password};
                environment.put(JMXConnector.CREDENTIALS, credent);
            }
            if (ssl) {
                environment.put(Context.SECURITY_PROTOCOL, "ssl");
                SslRMIClientSocketFactory clientSocketFactory = new SslRMIClientSocketFactory();
                environment.put(RMIConnectorServer.RMI_CLIENT_SOCKET_FACTORY_ATTRIBUTE, clientSocketFactory);
                environment.put("com.sun.jndi.rmi.factory.socket", clientSocketFactory);
            }
            
            jmxc = JMXConnectorFactory.connect(new JMXServiceURL(jmxUrl), environment);
            beanConn = jmxc.getMBeanServerConnection();
        }
        try {
            // Query MBean names, see #89 for reasons queryMBeans() is used instead of queryNames()
            Set<ObjectName> mBeanNames = new HashSet<>();
            for (ObjectName name : whitelistObjectNames) {
                for (ObjectInstance instance : beanConn.queryMBeans(name, null)) {
                    mBeanNames.add(instance.getObjectName());
                }
            }
            
            for (ObjectName name : blacklistObjectNames) {
                for (ObjectInstance instance : beanConn.queryMBeans(name, null)) {
                    mBeanNames.remove(instance.getObjectName());
                }
            }
            
            // Now that we have *only* the whitelisted mBeans, remove any old ones from the cache:
            jmxMBeanPropertyCache.onlyKeepMBeans(mBeanNames);
            
            for (ObjectName objectName : mBeanNames) {
                long start = System.nanoTime();
                scrapeBean(beanConn, objectName);
                LOGGER.fine("TIME: " + (System.nanoTime() - start) + " ns for " + objectName.toString());
            }
        } finally {
            if (jmxc != null) {
                jmxc.close();
            }
        }
    }
    
    private void scrapeBean(final MBeanServerConnection beanConn, final ObjectName mbeanName) {
        MBeanInfo info;
        try {
            info = beanConn.getMBeanInfo(mbeanName);
        } catch (IOException | JMException e) {
            logScrape(mbeanName.toString(), "getMBeanInfo Fail: " + e);
            return;
        }
        MBeanAttributeInfo[] attrInfos = info.getAttributes();
        
        Map<String, MBeanAttributeInfo> name2AttrInfo = new LinkedHashMap<>();
        for (MBeanAttributeInfo attr : attrInfos) {
            if (!attr.isReadable()) {
                logScrape(mbeanName, attr, "not readable");
                continue;
            }
            name2AttrInfo.put(attr.getName(), attr);
        }
        final AttributeList attributes;
        try {
            attributes = beanConn.getAttributes(mbeanName, name2AttrInfo.keySet().toArray(new String[0]));
            if (attributes == null) {
                logScrape(mbeanName.toString(), "getAttributes Fail: attributes are null");
                return;
            }
        } catch (InstanceNotFoundException | ReflectionException | IOException e) {
            logScrape(mbeanName, name2AttrInfo.keySet(), "Fail: " + e);
            return;
        }
        for (Attribute attribute : attributes.asList()) {
            MBeanAttributeInfo attr = name2AttrInfo.get(attribute.getName());
            logScrape(mbeanName, attr, "process");
            processBeanValue(mbeanName.getDomain(), jmxMBeanPropertyCache.getKeyPropertyList(mbeanName),
                    new LinkedList<>(), attr.getName(), attr.getType(), attr.getDescription(), attribute.getValue());
            
        }
    }
    
    /**
     * Recursive function for exporting the values of an mBean.
     * JMX is a very open technology, without any prescribed way of declaring mBeans
     * so this function tries to do a best-effort pass of getting the values/names
     * out in a way it can be processed elsewhere easily.
     */
    private void processBeanValue(final String domain, final Map<String, String> beanProperties,
                                  final LinkedList<String> attrKeysParam, final String attrName, final String attrTypeParam,
                                  final String attrDescription, final Object valueParam) {
        LinkedList<String> attrKeys = attrKeysParam;
        String attrType = attrTypeParam;
        Object value = valueParam;
        if (value == null) {
            logScrape(domain + beanProperties + attrName, "null");
        } else if (value instanceof Number || value instanceof String || value instanceof Boolean || value instanceof java.util.Date) {
            if (value instanceof java.util.Date) {
                attrType = "java.lang.Double";
                value = ((java.util.Date) value).getTime() / 1000.0;
            }
            logScrape(domain + beanProperties + attrName, value.toString());
            this.receiver.recordBean(domain, beanProperties, attrKeys, attrName, attrType, attrDescription, value);
        } else if (value instanceof CompositeData) {
            logScrape(domain + beanProperties + attrName, "compositedata");
            CompositeData composite = (CompositeData) value;
            CompositeType type = composite.getCompositeType();
            attrKeys = new LinkedList<>(attrKeys);
            attrKeys.add(attrName);
            for (String key : type.keySet()) {
                String typ = type.getType(key).getTypeName();
                Object valu = composite.get(key);
                processBeanValue(domain, beanProperties, attrKeys, key, typ, type.getDescription(), valu);
            }
        } else if (value instanceof TabularData) {
            // I don't pretend to have a good understanding of TabularData.
            // The real world usage doesn't appear to match how they were
            // meant to be used according to the docs. I've only seen them
            // used as 'key' 'value' pairs even when 'value' is itself a
            // CompositeData of multiple values.
            logScrape(domain + beanProperties + attrName, "tabulardata");
            TabularData tds = (TabularData) value;
            TabularType tt = tds.getTabularType();
            List<String> rowKeys = tt.getIndexNames();
            CompositeType type = tt.getRowType();
            Set<String> valueKeys = new TreeSet<>(type.keySet());
            valueKeys.removeAll(rowKeys);
            LinkedList<String> extendedAttrKeys = new LinkedList<>(attrKeys);
            extendedAttrKeys.add(attrName);
            for (Object valu : tds.values()) {
                if (valu instanceof CompositeData) {
                    CompositeData composite = (CompositeData) valu;
                    Map<String, String> rowKeyMap = new LinkedHashMap<>(beanProperties);
                    String resultKey;
                    for (String rowKey : rowKeys) {
                        resultKey = rowKey;
                        Object obj = composite.get(resultKey);
                        if (obj != null) {
                            // Nested tabulardata will repeat the 'key' label, so
                            // append a suffix to distinguish each.
                            while (rowKeyMap.containsKey(resultKey)) {
                                resultKey = resultKey + "_";
                            }
                            rowKeyMap.put(resultKey, obj.toString());
                        }
                    }
                    for (String valueIdx : valueKeys) {
                        LinkedList<String> attrNames = extendedAttrKeys;
                        String typ = type.getType(valueIdx).getTypeName();
                        String name = valueIdx;
                        if ("value".equalsIgnoreCase(valueIdx)) {
                            // Skip appending 'value' to the name
                            attrNames = attrKeys;
                            name = attrName;
                        }
                        processBeanValue(domain, rowKeyMap, attrNames, name, typ, type.getDescription(), composite.get(valueIdx));
                    }
                } else {
                    logScrape(domain, "not a correct tabulardata format");
                }
            }
        } else if (value.getClass().isArray()) {
            logScrape(domain, "arrays are unsupported");
        } else {
            logScrape(domain + beanProperties, attrType + " is not exported");
        }
    }
}

