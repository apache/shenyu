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
import org.junit.Test;

import javax.management.MalformedObjectNameException;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.regex.Pattern;

import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

/**
 * The type Jmx collector test.
 *
 * @author davidliu
 * @author  Young Bean
 */
public final class JmxCollectorTest {

    /**
     * case: Any JmxCollector instance invoke {@link JmxCollector#describe()} should get the same result.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     */
    @Test
    public void testAnyJmxCollectorInstanceInvokeDescribeMethodReturnTheSameResult() throws MalformedObjectNameException {
        final JmxCollector jmxCollectorInitWithEmptyJson = new JmxCollector(new JmxCollectorConfigObject().toConfigJson());
        final JmxCollector jmxCollectorInitWithNullObject = new JmxCollector(null);
        jmxCollectorInitWithEmptyJson.describe();
        assertThat(jmxCollectorInitWithEmptyJson.describe(), equalTo(jmxCollectorInitWithNullObject.describe()));
    }

    /**
     * case: Test init JmxConfig with startDelaySeconds, invoke collect method within startDelaySeconds.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     */
    @Test
    public void testInitJmxConfigWithStartWithDelaySecondsThenInvokeCollectMethodWithinStartDelaySeconds() throws MalformedObjectNameException {
        final int startDelaySeconds = 1;
        JmxCollectorConfigObject configObject = new JmxCollectorConfigObject();
        configObject.setStartDelaySeconds(startDelaySeconds);
        final JmxCollector jmxCollector = new JmxCollector(configObject.toConfigJson());
        // when collect within giving startDelaySeconds value, it should throw exception
        try {
            jmxCollector.collect();
        } catch (Exception e) {
            assertTrue(e instanceof IllegalStateException);
            assertThat("JMXCollector waiting for startDelaySeconds", equalTo(e.getMessage()));
        }
    }

    /**
     * case: Test init JmxConfig with startDelaySeconds, invoke collect method after startDelaySeconds.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     */
    @Test
    public void testInitJmxConfigWithStartWithDelaySecondsThenInvokeCollectMethodAfterStartDelaySeconds() throws MalformedObjectNameException {
        final int startDelaySeconds = 0;
        JmxCollectorConfigObject configObject = new JmxCollectorConfigObject();
        configObject.setStartDelaySeconds(startDelaySeconds);
        final JmxCollector jmxCollector = new JmxCollector(configObject.toConfigJson());
        assertNotNull(jmxCollector.collect());
    }

    /**
     * case: Test init JmxConfig with wrong startDelaySeconds value.
     *
     * @throws MalformedObjectNameException MalformedObjectNameException
     */
    @Test
    public void testInitJmxConfigWithWrongStartWithDelaySeconds() throws MalformedObjectNameException {
        // given startDelaySeconds wrong value
        try {
            new JmxCollector("{\"startDelaySeconds\": \"a\"}");
        } catch (Exception e) {
            assertTrue(e instanceof IllegalArgumentException);
            assertThat("Invalid number provided for startDelaySeconds", equalTo(e.getMessage()));
        }
        // additional coverage with none functional config
        new JmxCollector("{\"foo\": \"bar\"}");
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
        final String jmxHostPort = "127.0.0.1:9876";
        final String jmxUrl = "service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi";
        JmxCollectorConfigObject configObject = new JmxCollectorConfigObject();
        configObject.setHostPort(jmxHostPort);
        configObject.setJmxUrl(jmxUrl);
        try {
            new JmxCollector(configObject.toConfigJson());
        } catch (Exception e) {
            assertTrue(e instanceof IllegalArgumentException);
            assertThat("At most one of hostPort and jmxUrl must be provided", equalTo(e.getMessage()));
        }

        configObject.setJmxUrl(null);
        final JmxCollector jmxCollectorInitWithHostPort = new JmxCollector(configObject.toConfigJson());
        assertThat(jmxUrl, equalTo(this.dissectJmxConfigFromJmxCollector(jmxCollectorInitWithHostPort).getJmxUrl()));

        configObject.setHostPort(null);
        configObject.setJmxUrl(jmxUrl);
        final JmxCollector jmxCollectorInitWithJmxUrl = new JmxCollector(configObject.toConfigJson());
        assertThat(jmxUrl, equalTo(this.dissectJmxConfigFromJmxCollector(jmxCollectorInitWithJmxUrl).getJmxUrl()));
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
        final JmxCollectorConfigObject configObject = new JmxCollectorConfigObject();
        configObject.setStartDelaySeconds(0);
        configObject.setJmxUrl("service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi");
        configObject.setUsername("username");
        configObject.setPassword("password");
        configObject.setSsl(false);
        configObject.setLowercaseOutputName(true);
        configObject.setLowercaseOutputLabelNames(true);
        configObject.setWhitelistObjectNames(Collections.singletonList("JmxTest:name=wlo"));
        configObject.setBlacklistObjectNames(Collections.singletonList("JmxTest:name=blo"));
        JmxCollectorConfigObject.Rule configObjectRule = new JmxCollectorConfigObject.Rule();
        configObjectRule.setPattern("arp");
        configObjectRule.setName("name");
        configObjectRule.setValue("value");
        configObjectRule.setValueFactor(0.2D);
        configObjectRule.setHelp("help");
        configObjectRule.setAttrNameSnakeCase(true);
        configObjectRule.setType("UNTYPED");
        configObjectRule.setLabels(Collections.singletonMap("labelName", "labelValue"));
        configObject.setRules(Collections.singletonList(configObjectRule));

        JmxConfig jmxConfig = this.dissectJmxConfigFromJmxCollector(new JmxCollector(configObject.toConfigJson()));
        assertThat(configObject.getJmxUrl(), equalTo(jmxConfig.getJmxUrl()));
        assertThat(configObject.getUsername(), equalTo(jmxConfig.getUsername()));
        assertThat(configObject.getPassword(), equalTo(jmxConfig.getPassword()));
        assertFalse(jmxConfig.isSsl());
        assertTrue(jmxConfig.isLowercaseOutputLabelNames());
        assertTrue(jmxConfig.isLowercaseOutputName());
        assertThat(configObject.getStartDelaySeconds(), equalTo(jmxConfig.getStartDelaySeconds()));
        assertThat(1, equalTo(jmxConfig.getRules().size()));
        assertThat(1, equalTo(jmxConfig.getBlacklistObjectNames().size()));
        assertThat(1, equalTo(jmxConfig.getWhitelistObjectNames().size()));
        assertThat("JmxTest:name=blo", equalTo(jmxConfig.getBlacklistObjectNames().iterator().next().getCanonicalName()));
        assertThat("JmxTest:name=wlo", equalTo(jmxConfig.getWhitelistObjectNames().iterator().next().getCanonicalName()));

        JmxConfig.Rule rule = jmxConfig.getRules().iterator().next();
        assertThat(configObjectRule.getHelp(), equalTo(rule.getHelp()));
        assertThat(Pattern.compile("^.*(?:" + configObjectRule.getPattern() + ").*$").pattern(), equalTo(rule.getPattern().pattern()));
        assertThat(configObjectRule.getName(), equalTo(rule.getName()));
        assertThat(configObjectRule.getValue(), equalTo(rule.getValue()));
        assertThat(configObjectRule.getType(), equalTo(rule.getType().name()));
        assertTrue(rule.isAttrNameSnakeCase());
        assertThat(rule.getValueFactor(), equalTo(configObjectRule.getValueFactor()));
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
        final JmxCollectorConfigObject configObject = new JmxCollectorConfigObject();
        configObject.setStartDelaySeconds(0);
        configObject.setJmxUrl("service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi");
        configObject.setUsername("username");
        configObject.setPassword("password");
        configObject.setSsl(false);
        configObject.setLowercaseOutputName(true);
        configObject.setLowercaseOutputLabelNames(true);
        configObject.setWhitelistObjectNames(Collections.singletonList("JmxTest:name=wlo"));
        configObject.setBlacklistObjectNames(Collections.singletonList("JmxTest:name=blo"));
        JmxCollectorConfigObject.Rule configObjectRule = new JmxCollectorConfigObject.Rule();
        configObjectRule.setPattern("arp");
        configObjectRule.setName("name");
        configObjectRule.setLabels(Collections.singletonMap("labelName", "labelValue"));
        configObject.setRules(Collections.singletonList(configObjectRule));

        final String cfgJsonScene1 = configObject.toConfigJson();
        final JmxConfig jmxConfigScene1 = this.dissectJmxConfigFromJmxCollector(new JmxCollector(cfgJsonScene1));
        assertThat(1, equalTo(jmxConfigScene1.getRules().size()));
        assertThat(1.0D, equalTo(jmxConfigScene1.getRules().iterator().next().getValueFactor()));

        // giving rule wrong double value, default should acquire
        final String cfgJsonScene2 = "{\"startDelaySeconds\":0,\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi\",\"username\":\"username\",\"password\":\"password\","
                + "\"ssl\":false,\"lowercaseOutputName\":true,\"lowercaseOutputLabelNames\":true,\"whitelistObjectNames\":[\"JmxTest:name=wlo\"],"
                + "\"blacklistObjectNames\":[\"JmxTest:name=blo\"],\"rules\":[{\"name\": \"name\", \"pattern\":\"arp\", \"valueFactor\":\"value\", \"labels\": "
                + "{\"labelName\":\"labelValue\"}}]}";
        final JmxConfig jmxConfigScene2 = this.dissectJmxConfigFromJmxCollector(new JmxCollector(cfgJsonScene2));
        assertThat(1, equalTo(jmxConfigScene2.getRules().size()));
        assertThat(1.0D, equalTo(jmxConfigScene1.getRules().iterator().next().getValueFactor()));
    }

    /**
     * case: giving input then coverage with initialize JmxConfig failed on rule validation failed.
     */
    @Test
    public void testInitJmxConfigWithPartialPropsThenRuleValidationFailed() {
        // giving non value JmxConfig rule, rule validation should be failed
        final JmxCollectorConfigObject configObjectWithEmptyRules = new JmxCollectorConfigObject();
        configObjectWithEmptyRules.setStartDelaySeconds(0);
        configObjectWithEmptyRules.setJmxUrl("service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi");
        configObjectWithEmptyRules.setUsername("username");
        configObjectWithEmptyRules.setPassword("password");
        configObjectWithEmptyRules.setSsl(false);
        configObjectWithEmptyRules.setLowercaseOutputName(true);
        configObjectWithEmptyRules.setLowercaseOutputLabelNames(true);
        configObjectWithEmptyRules.setWhitelistObjectNames(Collections.singletonList("JmxTest:name=wlo"));
        configObjectWithEmptyRules.setBlacklistObjectNames(Collections.singletonList("JmxTest:name=blo"));
        configObjectWithEmptyRules.setRules(Collections.emptyList());

        try {
            new JmxCollector(configObjectWithEmptyRules.toConfigJson());
        } catch (Exception e) {
            assertTrue(e instanceof IllegalArgumentException);
            assertThat("Must provide name, if help or labels are given: {}", equalTo(e.getMessage()));
        }

        // giving non correct rule name and pattern, rule name and pattern validation should be failed
        final JmxCollectorConfigObject configObjectToTestPatternValidationFailed = new JmxCollectorConfigObject();
        configObjectToTestPatternValidationFailed.setStartDelaySeconds(0);
        configObjectToTestPatternValidationFailed.setJmxUrl("service:jmx:rmi:///jndi/rmi://127.0.0.1:9876/jmxrmi");
        configObjectToTestPatternValidationFailed.setUsername("username");
        configObjectToTestPatternValidationFailed.setPassword("password");
        configObjectToTestPatternValidationFailed.setSsl(false);
        configObjectToTestPatternValidationFailed.setLowercaseOutputName(true);
        configObjectToTestPatternValidationFailed.setLowercaseOutputLabelNames(true);
        configObjectToTestPatternValidationFailed.setWhitelistObjectNames(Collections.singletonList("JmxTest:name=wlo"));
        configObjectToTestPatternValidationFailed.setBlacklistObjectNames(Collections.singletonList("JmxTest:name=blo"));
        JmxCollectorConfigObject.Rule configObjectRule = new JmxCollectorConfigObject.Rule();
        configObjectRule.setName("name");
        configObjectToTestPatternValidationFailed.setRules(Collections.singletonList(configObjectRule));
        try {
            new JmxCollector(configObjectToTestPatternValidationFailed.toConfigJson());
        } catch (Exception e) {
            assertTrue(e instanceof IllegalArgumentException);
            assertTrue(e.getMessage().contains("Must provide pattern, if name is given:"));
        }
    }

    /**
     * Reflect to dissect JmxConfig instance from JmxCollector.
     *
     * @param jmxCollector giving JmxCollector instance
     * @return JmxConfig instance
     * @throws NoSuchFieldException   NoSuchFieldException
     * @throws IllegalAccessException IllegalAccessException
     */
    private JmxConfig dissectJmxConfigFromJmxCollector(final JmxCollector jmxCollector) throws NoSuchFieldException, IllegalAccessException {
        final Field configField = JmxCollector.class.getDeclaredField("config");
        configField.setAccessible(true);
        return (JmxConfig) configField.get(jmxCollector);
    }
}
