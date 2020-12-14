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

import org.dromara.soul.metrics.config.JmxConfig;
import org.dromara.soul.metrics.prometheus.impl.collector.support.JmxTestServer;
import org.dromara.soul.metrics.prometheus.impl.collector.support.MockDynamicMBean;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import javax.management.JMException;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * The type Jmx scraper test.
 */
public final class JmxScraperTest {
    
    private static JmxTestServer jmxTestServer;
    
    @BeforeClass
    public static void setup() throws JMException {
        jmxTestServer = new JmxTestServer(9876);
        jmxTestServer.start();
        jmxTestServer.register(new MockDynamicMBean("white list object", true), new ObjectName("JmxTest", "wk1", "wlo"));
        jmxTestServer.register(new MockDynamicMBean("black list object", true), new ObjectName("JmxTest", "name", "blo"));
        jmxTestServer.register(new MockDynamicMBean("white list object not readable", false), new ObjectName("JmxTest", "wk2", "wlo"));
        jmxTestServer.register(new MockDynamicMBean(null, true), new ObjectName("JmxTest", "wk3", "wlo"));
    }
    
    @AfterClass
    public static void tearDown() {
        jmxTestServer.stop();
    }
    
    /**
     * case: Giving empty jmxUrl, then invoke {@link JmxScraper#doScrape()}.
     *
     * @throws Exception the exception
     */
    @Test
    public void testJmxScraperWithEmptyJmxUrl() throws Exception {
        List<ObjectName> objectNames = new LinkedList<>();
        new JmxScraper("", "", "", false, objectNames, new LinkedList<>(), new StdoutWriter(), new JmxMBeanPropertyCache()).doScrape();
    }
    
    @Test
    public void testJmxScraperWithNonEmptyJmxUrl() throws Exception {
        // jmx config json
        String cfgJson = "{\"startDelaySeconds\":0,\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi\",\"username\":\"username\",\"password\":\"password\","
                + "\"ssl\":false,\"lowercaseOutputName\":true,\"lowercaseOutputLabelNames\":true,\"whitelistObjectNames\":[\"JmxTest:name=wlo\"],"
                + "\"blacklistObjectNames\":[\"JmxTest:name=blo\"],\"rules\":[{\"pattern\":\"o\",\"name\":\"name\",\"value\":\"1\",\"valueFactor\":0.2,\"help\":\"help\","
                + "\"attrNameSnakeCase\":true,\"type\":\"UNTYPED\", \"labels\": {\"labelName\":\"labelValue\"}}]}";
        
        final String mockJmxUrl = "service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi";
        final List<ObjectName> whiteListObjects = Arrays.asList(new ObjectName("JmxTest", "wk1", "wlo"), new ObjectName("JmxTest", "wk2", "wlo"), new ObjectName("JmxTest", "wk3", "wlo"));
        final List<ObjectName> blackListObjects = Collections.singletonList(new ObjectName("JmxTest", "name", "blo"));
        new JmxScraper(mockJmxUrl, "username", "password", false, whiteListObjects, blackListObjects, this.buildJmxCollectorReceiver(cfgJson), new JmxMBeanPropertyCache()).doScrape();
    }
    
    @Test
    public void testJmxScraperWithNonEmptyJmxUrlThenReceivedByReceiverDefaultExport() throws Exception {
        // jmx config json
        String cfgJson = "{\"startDelaySeconds\":0,\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi\",\"username\":\"username\",\"password\":\"password\","
                + "\"ssl\":false,\"lowercaseOutputName\":true,\"lowercaseOutputLabelNames\":true,\"whitelistObjectNames\":[\"JmxTest:name=wlo\"],"
                + "\"blacklistObjectNames\":[\"JmxTest:name=blo\"],\"rules\":[{\"pattern\":\"o\",\"name\":\"name\",\"value\":\"1\",\"valueFactor\":0.2,"
                + "\"attrNameSnakeCase\":true,\"type\":\"UNTYPED\"}]}";
        
        final String mockJmxUrl = "service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi";
        final List<ObjectName> whiteListObjects = Arrays.asList(new ObjectName("JmxTest", "wk1", "wlo"), new ObjectName("JmxTest", "wk2", "wlo"), new ObjectName("JmxTest", "wk3", "wlo"));
        final List<ObjectName> blackListObjects = Collections.singletonList(new ObjectName("JmxTest", "name", "blo"));
        
        JmxCollector jmxCollector = new JmxCollector(cfgJson);
        Class<JmxCollector.Receiver> receiverClass = JmxCollector.Receiver.class;
        Constructor<?>[] constructors = receiverClass.getDeclaredConstructors();
        constructors[0].setAccessible(true);
        
        final Field configField = JmxCollector.class.getDeclaredField("config");
        configField.setAccessible(true);
        JmxConfig jmxConfig = (JmxConfig) configField.get(jmxCollector);
        JmxConfig.Rule rule = jmxConfig.getRules().iterator().next();
        rule.setName(null);
        rule.setLabelValues(null);
        
        JmxCollector.Receiver receiver = (JmxCollector.Receiver) constructors[0].newInstance(jmxCollector);
        new JmxScraper(mockJmxUrl, "username", "password", false, whiteListObjects, blackListObjects, receiver, new JmxMBeanPropertyCache()).doScrape();
    }
    
    private JmxCollector.Receiver buildJmxCollectorReceiver(final String jmxCfgJson) throws MalformedObjectNameException, IllegalAccessException, InvocationTargetException,
            InstantiationException {
        JmxCollector jmxCollector = new JmxCollector(jmxCfgJson);
        Class<JmxCollector.Receiver> receiverClass = JmxCollector.Receiver.class;
        Constructor<?>[] constructors = receiverClass.getDeclaredConstructors();
        constructors[0].setAccessible(true);
        return (JmxCollector.Receiver) constructors[0].newInstance(jmxCollector);
    }
    
    private static class StdoutWriter implements MBeanReceiver {
        
        public void recordBean(final String domain, final Map<String, String> beanProperties,
                               final LinkedList<String> attrKeys, final String attrName,
                               final String attrType, final String attrDescription, final Object value) {
            System.out.println(domain + attrKeys + attrName + ": " + value);
        }
    }
}
