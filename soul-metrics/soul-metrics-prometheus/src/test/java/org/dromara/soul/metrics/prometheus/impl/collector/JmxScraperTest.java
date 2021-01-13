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
import org.dromara.soul.metrics.prometheus.impl.collector.support.JmxCollectorConfigObject;
import org.dromara.soul.metrics.prometheus.impl.collector.support.MockJmxServer;
import org.dromara.soul.metrics.prometheus.impl.collector.support.MockMetrics;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import javax.management.MBeanException;
import javax.management.ObjectName;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * The type Jmx scraper test.
 */
public final class JmxScraperTest {
    
    private static final int MOCK_JMX_SERVER_REGISTRY_PORT = 9876;
    
    private static MockJmxServer mockJmxServer;
    
    @BeforeClass
    public static void setup() throws Exception {
        mockJmxServer = new MockJmxServer(MOCK_JMX_SERVER_REGISTRY_PORT);
        mockJmxServer.start();
        mockJmxServer.register(new MockMetrics(11, "foo", new Date(), buildMockCompositeData(), buildMockTabularData(), new int[0], new String[0]),
                new ObjectName("JmxTest:name=mock1"));
    }
    
    @AfterClass
    public static void tearDown() {
        mockJmxServer.stop();
    }
    
    private static CompositeData buildMockCompositeData() throws Exception {
        ObjectName name = new ObjectName("com.sun.management:type=HotSpotDiagnostic");
        String operationName = "getVMOption";
        Object[] params = new Object[]{"SurvivorRatio"};
        String[] signature = new String[]{String.class.getName()};
        Object result = ManagementFactory.getPlatformMBeanServer().invoke(name, operationName, params, signature);
        return (CompositeDataSupport) result;
    }
    
    private static TabularData buildMockTabularData() throws MBeanException {
        try {
            CompositeType jobType = new CompositeType("Job", "Scheduler job",
                    new String[]{"Job", "Schedule"},
                    new String[]{"Job Name", "Job Scheduling"},
                    new OpenType[]{SimpleType.STRING, SimpleType.STRING});
            TabularType tableType = new TabularType("Jobs", "Tables of all jobs", jobType, new String[]{"Job"});
            TabularData table = new TabularDataSupport(tableType);
            CompositeData data = new CompositeDataSupport(jobType,
                    new String[]{"Job", "Schedule"},
                    new Object[]{"JobName", "Job Schedule"});
            table.put(data);
            return table;
        } catch (Exception e) {
            throw new MBeanException(null, e.toString());
        }
    }
    
    /**
     * case: Giving empty jmxUrl, then invoke {@link JmxScraper#doScrape()}.
     *
     * @throws Exception the Exception
     */
    @Test
    public void testJmxScraperWithEmptyJmxUrl() throws Exception {
        List<ObjectName> objectNames = new LinkedList<>();
        new JmxScraper("", "", "", false, objectNames, new LinkedList<>(), new StdoutWriter(), new JmxMBeanPropertyCache()).doScrape();
    }
    
    /**
     * case: Giving non empty jmxUrl, then invoke {@link JmxScraper#doScrape()}.
     *
     * @throws Exception the Exception
     */
    @Test
    public void testJmxScraperWithNonEmptyJmxUrl() throws Exception {
        final String mockJmxUrl = "service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi";
        final List<ObjectName> whiteListObjects = Collections.singletonList(new ObjectName("JmxTest:name=mock*"));
        final List<ObjectName> blackListObjects = Collections.singletonList(new ObjectName("JmxTest:name=b*"));
        // prepare jmx config
        JmxCollectorConfigObject configObject = new JmxCollectorConfigObject();
        configObject.setStartDelaySeconds(0);
        configObject.setJmxUrl(mockJmxUrl);
        configObject.setUsername("username");
        configObject.setPassword("password");
        configObject.setSsl(false);
        configObject.setLowercaseOutputName(true);
        configObject.setLowercaseOutputLabelNames(true);
        configObject.setWhitelistObjectNames(Collections.singletonList("JmxTest:name=mock*"));
        configObject.setBlacklistObjectNames(Collections.singletonList("JmxTest:name=blo"));
        JmxCollectorConfigObject.Rule configRule = new JmxCollectorConfigObject.Rule();
        configRule.setName("name");
        configRule.setPattern("o");
        configRule.setValue("1");
        configRule.setValueFactor(0.2);
        configRule.setHelp("help");
        configRule.setAttrNameSnakeCase(true);
        configRule.setType("UNTYPED");
        configRule.setLabels(Collections.singletonMap("labelName", "labelValue"));
        configObject.setRules(Collections.singletonList(configRule));
        
        JmxCollector.Receiver receiver = this.reflectBuildJmxCollectorReceiver(new JmxCollector(configObject.toConfigJson()));
        new JmxScraper(mockJmxUrl, "username", "password", false, whiteListObjects, blackListObjects, receiver, new JmxMBeanPropertyCache()).doScrape();
    }
    
    /**
     * case: Giving non empty jmxUrl, and reflect to set rule to null then invoke {@link JmxScraper#doScrape()}.
     *
     * @throws Exception the Exception
     */
    @Test
    public void testJmxScraperWithNonEmptyJmxUrlThenReceivedByReceiverDefaultExport() throws Exception {
        final String mockJmxUrl = "service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi";
        final List<ObjectName> whiteListObjects = Collections.singletonList(new ObjectName("JmxTest:name=mock*"));
        final List<ObjectName> blackListObjects = Collections.singletonList(new ObjectName("JmxTest:name=blo"));
        
        // prepare jmx config
        JmxCollectorConfigObject configObject = new JmxCollectorConfigObject();
        configObject.setStartDelaySeconds(0);
        configObject.setJmxUrl(mockJmxUrl);
        configObject.setUsername("username");
        configObject.setPassword("password");
        configObject.setSsl(false);
        configObject.setLowercaseOutputName(true);
        configObject.setLowercaseOutputLabelNames(true);
        configObject.setWhitelistObjectNames(Collections.singletonList("JmxTest:name=mock*"));
        configObject.setBlacklistObjectNames(Collections.singletonList("JmxTest:name=blo"));
        JmxCollectorConfigObject.Rule configRule = new JmxCollectorConfigObject.Rule();
        configRule.setPattern("o");
        configRule.setName("name");
        configRule.setValue("1");
        configRule.setValueFactor(0.2);
        configRule.setAttrNameSnakeCase(true);
        configRule.setType("UNTYPED");
        configObject.setRules(Collections.singletonList(configRule));
        
        JmxCollector jmxCollector = new JmxCollector(configObject.toConfigJson());
        Class<JmxCollector.Receiver> receiverClass = JmxCollector.Receiver.class;
        Constructor<?>[] constructors = receiverClass.getDeclaredConstructors();
        constructors[0].setAccessible(true);
        
        final Field configField = JmxCollector.class.getDeclaredField("config");
        configField.setAccessible(true);
        JmxConfig jmxConfig = (JmxConfig) configField.get(jmxCollector);
        JmxConfig.Rule rule = jmxConfig.getRules().iterator().next();
        rule.setName(null);
        rule.setLabelValues(null);
        
        JmxCollector.Receiver receiver = this.reflectBuildJmxCollectorReceiver(jmxCollector);
        new JmxScraper(mockJmxUrl, "username", "password", false, whiteListObjects, blackListObjects, receiver, new JmxMBeanPropertyCache()).doScrape();
    }
    
    private JmxCollector.Receiver reflectBuildJmxCollectorReceiver(final JmxCollector jmxCollector) throws Exception {
        Constructor<?>[] constructors = JmxCollector.Receiver.class.getDeclaredConstructors();
        constructors[0].setAccessible(true);
        return (JmxCollector.Receiver) constructors[0].newInstance(jmxCollector);
    }
    
    private static class StdoutWriter implements MBeanReceiver {
        public void recordBean(final String domain, final Map<String, String> beanProperties,
                               final LinkedList<String> attrKeys, final String attrName,
                               final String attrType, final String attrDescription, final Object value) {
            // FIXME Have to assert the bean.
        }
    }
}
