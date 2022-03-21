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

import java.util.Properties;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The Test Case For MetricsConfig.
 */
public final class MetricsConfigTest {

    private MetricsConfig metricsConfig;

    @BeforeEach
    public void setUp() {
        Properties properties = new Properties();
        properties.setProperty("key", "value");
        metricsConfig = new MetricsConfig("prometheus", "host", 9999, Boolean.TRUE,
                10, "jmxConfig", properties);
    }

    @Test
    public void getMetricsName() {
        assertThat(metricsConfig.getMetricsName(), is("prometheus"));
    }

    @Test
    public void getHost() {
        assertThat(metricsConfig.getHost(), is("host"));
    }

    @Test
    public void getPort() {
        assertThat(metricsConfig.getPort(), is(9999));
    }

    @Test
    public void getAsync() {
        assertTrue(metricsConfig.getAsync());
    }

    @Test
    public void getThreadCount() {
        assertThat(metricsConfig.getThreadCount(), is(10));
    }

    @Test
    public void getJmxConfig() {
        assertThat(metricsConfig.getJmxConfig(), is("jmxConfig"));
    }

    @Test
    public void getProps() {
        assertThat(metricsConfig.getProps().getProperty("key"), is("value"));
    }

    @Test
    public void testToString() {
        assertThat(metricsConfig.toString(), is("MetricsConfig(metricsName=prometheus, host=host, port=9999, async=true, threadCount=10, jmxConfig=jmxConfig, props={key=value})"));
    }

    @Test
    public void setMetricsName() {
        metricsConfig.setMetricsName("prometheus");
        assertThat(metricsConfig.getMetricsName(), is("prometheus"));
    }

    @Test
    public void setHost() {
        metricsConfig.setHost("192.168.1.1");
        assertThat(metricsConfig.getHost(), is("192.168.1.1"));
    }

    @Test
    public void setPort() {
        metricsConfig.setPort(9999);
        assertThat(metricsConfig.getPort(), is(9999));
    }

    @Test
    public void setAsync() {
        metricsConfig.setAsync(Boolean.FALSE);
        assertThat(metricsConfig.getAsync(), is(Boolean.FALSE));
    }

    @Test
    public void setThreadCount() {
        metricsConfig.setThreadCount(100);
        assertThat(metricsConfig.getThreadCount(), is(100));
    }

    @Test
    public void setJmxConfig() {
        metricsConfig.setJmxConfig("jmxConfig");
        assertThat(metricsConfig.getJmxConfig(), is("jmxConfig"));
    }

    @Test
    public void setProps() {
        Properties properties = new Properties();
        properties.setProperty("key", "value");
        metricsConfig.setProps(properties);
        assertThat(metricsConfig.getProps().getProperty("key"), is("value"));
    }

    @Test
    public void testEquals() {
        Properties properties = new Properties();
        properties.setProperty("key", "value");
        MetricsConfig metricsConfig1 = new MetricsConfig("prometheus", "host", 9999, Boolean.TRUE,
                10, "jmxConfig", properties);
        assertThat(metricsConfig1, is(metricsConfig));
    }

    @Test
    public void testHashCode() {
        Properties properties = new Properties();
        properties.setProperty("key", "value");
        MetricsConfig metricsConfig1 = new MetricsConfig("prometheus", "host", 9999, Boolean.TRUE,
                10, "jmxConfig", properties);
        assertThat(metricsConfig1.hashCode(), is(metricsConfig.hashCode()));
    }
}
