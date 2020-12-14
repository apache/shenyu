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
import org.junit.Assert;
import org.junit.Test;

import javax.management.MalformedObjectNameException;
import java.lang.reflect.Field;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

/**
 * The type Jmx collector test.
 */
public final class JmxCollectorTest {
    
    /**
     * case: Any JmxCollector instance invoke {@link JmxCollector#describe()} should get the same result.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     */
    @Test
    public void testAnyJmxCollectorInstanceInvokeDescribeMethodReturnTheSameResult() throws MalformedObjectNameException {
        final String configJson = "{}";
        final JmxCollector jmxCollectorInitWithEmptyJson = new JmxCollector(configJson);
        final JmxCollector jmxCollectorInitWithNullObject = new JmxCollector(null);
        jmxCollectorInitWithEmptyJson.describe();
        Assert.assertEquals(jmxCollectorInitWithEmptyJson.describe(), jmxCollectorInitWithNullObject.describe());
    }
    
    /**
     * case: Test init JmxConfig with startDelaySeconds, invoke collect method within startDelaySeconds.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     * @throws InterruptedException         InterruptedException
     */
    @Test
    public void testInitJmxConfigWithStartWithDelaySecondsThenInvokeCollectMethodWithinStartDelaySeconds() throws MalformedObjectNameException, InterruptedException {
        // given correct value
        final String configJson = "{\"startDelaySeconds\": 1}";
        final JmxCollector jmxCollector = new JmxCollector(configJson);
        // when collect within giving startDelaySeconds value, it should throw exception
        try {
            jmxCollector.collect();
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IllegalStateException);
            Assert.assertEquals("JMXCollector waiting for startDelaySeconds", e.getMessage());
        }
    }
    
    /**
     * case: Test init JmxConfig with startDelaySeconds, invoke collect method after startDelaySeconds.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     * @throws InterruptedException         InterruptedException
     */
    @Test
    public void testInitJmxConfigWithStartWithDelaySecondsThenInvokeCollectMethodAfterStartDelaySeconds() throws MalformedObjectNameException, InterruptedException {
        final String configJson = "{\"startDelaySeconds\": 1}";
        final JmxCollector jmxCollector = new JmxCollector(configJson);
        TimeUnit.SECONDS.sleep(1);
        // after startDelaySeconds, it should return
        Assert.assertNotNull(jmxCollector.collect());
    }
    
    /**
     * case: Test init JmxConfig with wrong startDelaySeconds value.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     */
    @Test
    public void testInitJmxConfigWithWrongStartWithDelaySeconds() throws MalformedObjectNameException {
        // given wrong value
        try {
            new JmxCollector("{\"startDelaySeconds\": \"a\"}");
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("Invalid number provided for startDelaySeconds", e.getMessage());
        }
        // additional coverage with none functional config
        new JmxCollector("{\"hello\": \"world\"}");
    }
    
    /**
     * case: Test init JmxConfig with hostPort or jmxUrl, then compare inner JmxConfig instance with inbound argument value.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     * @throws NoSuchFieldException         NoSuchFieldException
     * @throws IllegalAccessException       IllegalAccessException
     */
    @Test
    public void testInitJmxConfigWithHostPortAndJmxUrl() throws MalformedObjectNameException, NoSuchFieldException, IllegalAccessException {
        // when config both hostPort and jmxUrl, exception should be thrown
        try {
            new JmxCollector("{\"hostPort\": \"127.0.0.1:9876\", \"jmxUrl\": \"jmx\"}");
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("At most one of hostPort and jmxUrl must be provided", e.getMessage());
        }
        
        JmxCollector jmxCollectorInitWithHostPort = new JmxCollector("{\"hostPort\": \"127.0.0.1:9876\"}");
        Assert.assertEquals("service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi", this.dissectJmxConfigFromJmxCollector(jmxCollectorInitWithHostPort).getJmxUrl());
        
        JmxCollector jmxCollectorInitWithJmxUrl = new JmxCollector("{\"jmxUrl\": \"service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi\"}");
        Assert.assertEquals("service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi", this.dissectJmxConfigFromJmxCollector(jmxCollectorInitWithJmxUrl).getJmxUrl());
    }
    
    /**
     * case: Initialize with giving jmx config json, and then the inner jmx config of JmxCollector instance should have the same property.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     * @throws NoSuchFieldException         NoSuchFieldException
     * @throws IllegalAccessException       IllegalAccessException
     */
    @Test
    public void testInitJmxConfigWithAllPropsInnerJmxConfigPropShouldEqualInbound() throws MalformedObjectNameException, NoSuchFieldException, IllegalAccessException {
        String cfgJson = "{\"startDelaySeconds\":0,\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi\",\"username\":\"username\",\"password\":\"password\","
                + "\"ssl\":false,\"lowercaseOutputName\":true,\"lowercaseOutputLabelNames\":true,\"whitelistObjectNames\":[\"JmxTest:name=wlo\"],"
                + "\"blacklistObjectNames\":[\"JmxTest:name=blo\"],\"rules\":[{\"pattern\":\"arp\",\"name\":\"name\",\"value\":\"value\",\"valueFactor\":0.2,\"help\":\"help\","
                + "\"attrNameSnakeCase\":true,\"type\":\"UNTYPED\", \"labels\": {\"labelName\":\"labelValue\"}}]}";
        
        JmxConfig jmxConfig = this.dissectJmxConfigFromJmxCollector(new JmxCollector(cfgJson));
        Assert.assertEquals("service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi", jmxConfig.getJmxUrl());
        Assert.assertEquals("username", jmxConfig.getUsername());
        Assert.assertEquals("password", jmxConfig.getPassword());
        Assert.assertFalse(jmxConfig.isSsl());
        Assert.assertTrue(jmxConfig.isLowercaseOutputLabelNames());
        Assert.assertTrue(jmxConfig.isLowercaseOutputName());
        Assert.assertEquals(0, (int) jmxConfig.getStartDelaySeconds());
        Assert.assertEquals(1, jmxConfig.getRules().size());
        Assert.assertEquals(1, jmxConfig.getBlacklistObjectNames().size());
        Assert.assertEquals(1, jmxConfig.getWhitelistObjectNames().size());
        Assert.assertEquals("JmxTest:name=blo", jmxConfig.getBlacklistObjectNames().iterator().next().getCanonicalName());
        Assert.assertEquals("JmxTest:name=wlo", jmxConfig.getWhitelistObjectNames().iterator().next().getCanonicalName());
        JmxConfig.Rule rule = jmxConfig.getRules().iterator().next();
        
        Assert.assertEquals("help", rule.getHelp());
        Assert.assertEquals(Pattern.compile("^.*(?:arp).*$").pattern(), rule.getPattern().pattern());
        Assert.assertEquals("name", rule.getName());
        Assert.assertEquals("value", rule.getValue());
        Assert.assertEquals(JmxConfig.Type.UNTYPED, rule.getType());
        Assert.assertTrue(rule.isAttrNameSnakeCase());
        Assert.assertEquals(0.2D, rule.getValueFactor(), 0.0);
    }
    
    /**
     * case: giving jmx config json, initialize JmxCollector, then compare whether default JmxConfig.Rule default valueFactor acquired.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     * @throws NoSuchFieldException         NoSuchFieldException
     * @throws IllegalAccessException       IllegalAccessException
     */
    @Test
    public void testInitJmxConfigWithPartialPropsThenCompareJmxConfigRuleWhetherAcquireDefaultValues() throws MalformedObjectNameException, NoSuchFieldException,
            IllegalAccessException {
        // not give rule value factor, default should acquire
        final String cfgJsonScene1 = "{\"startDelaySeconds\":0,\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi\",\"username\":\"username\",\"password\":\"password\","
                + "\"ssl\":false,\"lowercaseOutputName\":true,\"lowercaseOutputLabelNames\":true,\"whitelistObjectNames\":[\"JmxTest:name=wlo\"],"
                + "\"blacklistObjectNames\":[\"JmxTest:name=blo\"],\"rules\":[{\"name\": \"name\", \"pattern\":\"arp\", \"labels\":{\"labelName\":\"labelValue\"}}]}";
        JmxConfig jmxConfigScene1 = this.dissectJmxConfigFromJmxCollector(new JmxCollector(cfgJsonScene1));
        Assert.assertEquals(1, jmxConfigScene1.getRules().size());
        Assert.assertEquals(1.0D, jmxConfigScene1.getRules().iterator().next().getValueFactor(), 0.0);
        // giving rule wrong double value, default should acquire
        final String cfgJsonScene2 = "{\"startDelaySeconds\":0,\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi\",\"username\":\"username\",\"password\":\"password\","
                + "\"ssl\":false,\"lowercaseOutputName\":true,\"lowercaseOutputLabelNames\":true,\"whitelistObjectNames\":[\"JmxTest:name=wlo\"],"
                + "\"blacklistObjectNames\":[\"JmxTest:name=blo\"],\"rules\":[{\"name\": \"name\", \"pattern\":\"arp\", \"valueFactor\":\"value\", \"labels\": "
                + "{\"labelName\":\"labelValue\"}}]}";
        JmxConfig jmxConfigScene2 = this.dissectJmxConfigFromJmxCollector(new JmxCollector(cfgJsonScene2));
        Assert.assertEquals(1, jmxConfigScene2.getRules().size());
        Assert.assertEquals(1.0D, jmxConfigScene1.getRules().iterator().next().getValueFactor(), 0.0);
    }
    
    /**
     * case: giving input then coverage with initialize JmxConfig failed on rule validation failed.
     */
    @Test
    public void testInitJmxConfigWithPartialPropsThenRuleValidationFailed() {
        // giving non value JmxConfig rule, rule validation should be failed
        final String cfgJsonScene3 = "{\"startDelaySeconds\":0,\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi\",\"username\":\"username\","
                + "\"password\":\"password\",\"ssl\":false,\"lowercaseOutputName\":true,\"lowercaseOutputLabelNames\":true,\"whitelistObjectNames\":[\"JmxTest:name=wlo\"],"
                + "\"blacklistObjectNames\":[\"JmxTest:name=blo\"],\"rules\":[{}]}";
        try {
            new JmxCollector(cfgJsonScene3);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("Must provide name, if help or labels are given: {}", e.getMessage());
        }
        // giving non correct rule name and pattern, rule name and pattern validation should be failed
        final String cfgJsonScene4 = "{\"startDelaySeconds\":0,\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi\",\"username\":\"username\","
                + "\"password\":\"password\",\"ssl\":false,\"lowercaseOutputName\":true,\"lowercaseOutputLabelNames\":true,\"whitelistObjectNames\":[\"JmxTest:name=wlo\"],"
                + "\"blacklistObjectNames\":[\"JmxTest:name=blo\"],\"rules\":[{\"name\": \"name\"}]}";
        try {
            new JmxCollector(cfgJsonScene4);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertTrue(e.getMessage().contains("Must provide pattern, if name is given:"));
        }
    }
    
    /**
     * Reflect to dissect JmxConfig instance from JmxCollector
     *
     * @param jmxCollector giving JmxCollector instance
     * @return JmxConfig instance
     * @throws NoSuchFieldException   NoSuchFieldException
     * @throws IllegalAccessException IllegalAccessException
     */
    private JmxConfig dissectJmxConfigFromJmxCollector(final JmxCollector jmxCollector) throws NoSuchFieldException, IllegalAccessException {
        Field configField = JmxCollector.class.getDeclaredField("config");
        configField.setAccessible(true);
        return (JmxConfig) configField.get(jmxCollector);
    }
    
}
