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

package org.dromara.soul.metrics.config;

import org.junit.Before;
import org.junit.Test;

import java.util.Properties;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

/**
 * The Test Case For MetricsConfig.
 *
 * @author nuo-promise
 **/
public final class MetricsConfigTest {

    private MetricsConfig metricsConfig;

    @Before
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
    }

    @Test
    public void setAsync() {
    }

    @Test
    public void setThreadCount() {
    }

    @Test
    public void setJmxConfig() {
    }

    @Test
    public void setProps() {
    }

    @Test
    public void testEquals() {
    }

    @Test
    public void testHashCode() {
    }
}
