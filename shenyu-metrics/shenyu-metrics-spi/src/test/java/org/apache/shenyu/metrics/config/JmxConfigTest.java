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

package org.apache.shenyu.metrics.config;

import org.apache.shenyu.metrics.config.JmxConfig.Rule;
import org.apache.shenyu.metrics.config.JmxConfig.Type;
import org.junit.Before;
import org.junit.Test;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import java.util.Collections;
import java.util.regex.Pattern;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

/**
 * The Test Case For JmxConfig.
 */
public final class JmxConfigTest {

    private JmxConfig jmxConfig;

    @Before
    public void setUp() throws MalformedObjectNameException {
        jmxConfig = new JmxConfig();
        jmxConfig.setStartDelaySeconds(10);
        jmxConfig.setJmxUrl("http://");
        jmxConfig.setUsername("promise");
        jmxConfig.setPassword("promise");
        jmxConfig.setSsl(Boolean.TRUE);
        jmxConfig.setLowercaseOutputName(Boolean.FALSE);
        jmxConfig.setLowercaseOutputLabelNames(Boolean.TRUE);
        ObjectName objectName1 = ObjectName.getInstance("promise1", "promise1", "10");
        ObjectName objectName2 = ObjectName.getInstance("promise1", "promise2", "11");
        jmxConfig.setWhitelistObjectNames(Collections.singletonList(objectName1));
        jmxConfig.setBlacklistObjectNames(Collections.singletonList(objectName2));
        Rule rule = new Rule();
        rule.setPattern(Pattern.compile("\\w+"));
        rule.setName("rule");
        rule.setValue("value");
        rule.setValueFactor(1.0);
        rule.setHelp("help");
        rule.setAttrNameSnakeCase(Boolean.TRUE);
        rule.setType(Type.UNTYPED);
        rule.setLabelNames(Collections.singletonList("labels"));
        rule.setLabelValues(Collections.singletonList("values"));
        jmxConfig.setRules(Collections.singletonList(rule));
    }

    @Test
    public void getStartDelaySeconds() {
        assertThat(jmxConfig.getStartDelaySeconds(), is(10));
    }

    @Test
    public void getJmxUrl() {
        assertThat(jmxConfig.getJmxUrl(), is("http://"));
    }

    @Test
    public void getUsername() {
        assertThat(jmxConfig.getUsername(), is("promise"));
    }

    @Test
    public void getPassword() {
        assertThat(jmxConfig.getPassword(), is("promise"));
    }

    @Test
    public void isSsl() {
        assertThat(jmxConfig.isSsl(), is(Boolean.TRUE));
    }

    @Test
    public void isLowercaseOutputName() {
        assertFalse(jmxConfig.isLowercaseOutputName());
    }

    @Test
    public void isLowercaseOutputLabelNames() {
        assertTrue(jmxConfig.isLowercaseOutputLabelNames());
    }

    @Test
    public void getWhitelistObjectNames() {
        assertThat(jmxConfig.getWhitelistObjectNames().get(0).toString(), is("promise1:promise1=10"));
    }

    @Test
    public void getBlacklistObjectNames() {
        assertThat(jmxConfig.getBlacklistObjectNames().get(0).toString(), is("promise1:promise2=11"));
    }

    @Test
    public void getRules() {
        assertThat(jmxConfig.getRules().get(0).toString(), is("JmxConfig.Rule(pattern=\\w+, name=rule, "
                + "value=value, valueFactor=1.0, help=help, attrNameSnakeCase=true, type=UNTYPED, labelNames=[labels], "
                + "labelValues=[values])"));
    }

    @Test
    public void testToString() {
        assertThat(jmxConfig.toString(),
               is("JmxConfig(startDelaySeconds=10, jmxUrl=http://, username=promise, "
                       + "password=promise, ssl=true, lowercaseOutputName=false, lowercaseOutputLabelNames=true, "
                       + "whitelistObjectNames=[promise1:promise1=10], blacklistObjectNames=[promise1:promise2=11], "
                       + "rules=[JmxConfig.Rule(pattern=\\w+, name=rule, value=value, valueFactor=1.0, help=help, "
                       + "attrNameSnakeCase=true, type=UNTYPED, labelNames=[labels], labelValues=[values])])"));
    }
}
