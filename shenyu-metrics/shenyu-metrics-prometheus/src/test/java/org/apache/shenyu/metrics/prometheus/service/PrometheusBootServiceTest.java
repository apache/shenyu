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

package org.apache.shenyu.metrics.prometheus.service;

import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.exporter.HTTPServer;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.metrics.config.MetricsConfig;
import org.apache.shenyu.metrics.prometheus.register.PrometheusMetricsRegister;

import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for PrometheusMetricsTrackerManager.
 */
public final class PrometheusBootServiceTest {

    private static final PrometheusBootService PROMETHEUS_BOOT_SERVICE = new PrometheusBootService();

    @BeforeEach
    public void init() {
        CollectorRegistry.defaultRegistry.clear();
    }
    
    @Test
    public void testRegistered() throws NoSuchFieldException, IllegalAccessException {
        AtomicBoolean registered = (AtomicBoolean) ReflectUtils.getFieldValue(PROMETHEUS_BOOT_SERVICE, "registered");
        registered.set(true);
        String jmxConfig = GsonUtils.getInstance().toJson("whitelistObjectNames:org.apache.cassandra.metrics:type=ColumnFamily");
        MetricsConfig metricsConfig = new MetricsConfig("test", "", 10119, false, 1, jmxConfig, null);
        PROMETHEUS_BOOT_SERVICE.start(metricsConfig, new PrometheusMetricsRegister());
        Field field = PrometheusBootService.class.getDeclaredField("server");
        field.setAccessible(true);
        HTTPServer httpServer = (HTTPServer) field.get(PROMETHEUS_BOOT_SERVICE);
        assertNotNull(httpServer);
        assertThat(httpServer.getPort(), is(10119));
        assertTrue(PROMETHEUS_BOOT_SERVICE.getRegistered().get());
    }
    
    @AfterAll
    public static void close() {
        PROMETHEUS_BOOT_SERVICE.stop();
    }
}
