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

package org.apache.shenyu.plugin.metrics.reporter;

import io.prometheus.client.Counter;
import io.prometheus.client.Gauge;
import io.prometheus.client.Histogram;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.plugin.metrics.config.Metric;
import org.apache.shenyu.plugin.metrics.enums.MetricType;
import org.apache.shenyu.plugin.metrics.prometheus.PrometheusMetricsRegister;
import org.apache.shenyu.plugin.metrics.spi.MetricsRegister;
import org.junit.FixMethodOrder;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.runners.MethodSorters;
import org.springframework.util.CollectionUtils;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * The Test Case For MetricsReporter.
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class MetricsReporterTest {

    private static final String DOCUMENT = "testDocument";

    private static MetricsRegister metricsRegister;

    @BeforeAll
    public static void setUp() {
        metricsRegister = new PrometheusMetricsRegister();
        MetricsReporter.register(metricsRegister);
    }

    @Test
    public void testRegister() throws Exception {
        Map<String, Counter> map1 = getPrivateField(metricsRegister, "COUNTER_MAP", Map.class);
        Assertions.assertEquals(map1.size(), 9);
        Map<String, Histogram> map2 = getPrivateField(metricsRegister, "HISTOGRAM_MAP", Map.class);
        Assertions.assertEquals(map2.size(), 3);
        List<String> labels = new ArrayList<>();
        labels.add("shenyu_request_total");
        Collection<Metric> metrics = new ArrayList<>();
        metrics.add(new Metric(MetricType.COUNTER, "name1", DOCUMENT, labels));
        metrics.add(new Metric(MetricType.GAUGE, "name2", DOCUMENT, labels));
        metrics.add(new Metric(MetricType.HISTOGRAM, "name3", DOCUMENT, labels));
        MetricsReporter.registerMetrics(metrics);
        Map<String, Counter> map3 = getPrivateField(metricsRegister, "COUNTER_MAP", Map.class);
        Assertions.assertEquals(map3.size(), 10);
        Map<String, Histogram> map4 = getPrivateField(metricsRegister, "HISTOGRAM_MAP", Map.class);
        Assertions.assertEquals(map4.size(), 4);
        Map<String, Gauge> map5 = getPrivateField(metricsRegister, "GAUGE_MAP", Map.class);
        Assertions.assertEquals(map5.size(), 3);
    }

    @Test
    public void testGauge() throws Exception {
        String name1 = "request_throw_total1";
        String name2 = "request_throw_total2";
        String[] labelNames = new String[] {"name1", "name2"};
        MetricsReporter.registerGauge(name1, labelNames, "shenyu request total count");
        MetricsReporter.registerGauge(name2, "shenyu request total count");
        MetricsReporter.gaugeIncrement(name1, labelNames);
        Field field1 = metricsRegister.getClass().getDeclaredField("GAUGE_MAP");
        field1.setAccessible(true);
        Map<String, Gauge> map1 = (Map<String, Gauge>) field1.get(metricsRegister);
        Gauge gauge1 = map1.get(name1);
        Assertions.assertEquals(gauge1.labels(labelNames).get(), 1.0);
        MetricsReporter.gaugeIncrement(name2);
        Field field2 = metricsRegister.getClass().getDeclaredField("GAUGE_MAP");
        field2.setAccessible(true);
        Map<String, Gauge> map2 = (Map<String, Gauge>) field2.get(metricsRegister);
        Gauge gauge2 = map2.get(name2);
        Assertions.assertEquals(gauge2.get(), 1.0);
        MetricsReporter.gaugeDecrement(name1, labelNames);
        Field field3 = metricsRegister.getClass().getDeclaredField("GAUGE_MAP");
        field3.setAccessible(true);
        Map<String, Gauge> map3 = (Map<String, Gauge>) field3.get(metricsRegister);
        Gauge gauge3 = map3.get(name1);
        Assertions.assertEquals(gauge3.labels(labelNames).get(), 0.0);
        MetricsReporter.gaugeDecrement(name2);
        Field field4 = metricsRegister.getClass().getDeclaredField("GAUGE_MAP");
        field4.setAccessible(true);
        Map<String, Gauge> map4 = (Map<String, Gauge>) field4.get(metricsRegister);
        Gauge gauge4 = map4.get(name2);
        Assertions.assertEquals(gauge4.get(), 0.0);
    }

    @Test
    public void testCounter() throws Exception {
        String counterName1 = "request_total1";
        String counterName2 = "request_total2";
        String[] labelNames = new String[]{"name1", "name2"};
        MetricsReporter.registerCounter(counterName1, DOCUMENT);
        MetricsReporter.registerCounter(counterName2, labelNames, DOCUMENT);
        MetricsReporter.counterIncrement(counterName1);
        Field field1 = metricsRegister.getClass().getDeclaredField("COUNTER_MAP");
        field1.setAccessible(true);
        Map<String, Counter> counterMap = getPrivateField(metricsRegister, "COUNTER_MAP", Map.class);
        Assertions.assertEquals(counterMap.get(counterName1).get(), 1);
        MetricsReporter.counterIncrement(counterName2, labelNames);
        Assertions.assertEquals(counterMap.get(counterName2).labels(labelNames).get(), 1);
        MetricsReporter.counterIncrement(counterName2, labelNames, 2);
        Assertions.assertEquals(counterMap.get(counterName2).labels(labelNames).get(), 3);
    }

    private <T> T getPrivateField(final Object object, final String fieldName, final Class<T> fieldType) throws NoSuchFieldException, IllegalAccessException {
        Field field = object.getClass().getDeclaredField(fieldName);
        field.setAccessible(true);
        return fieldType.cast(field.get(object));
    }

    @Test
    public void testRecordTime() {
        String name1 = "requests_latency_histogram_millis1";
        String name2 = "requests_latency_histogram_millis2";
        String[] labelNames = new String[] {"name"};
        MetricsReporter.registerHistogram(name1, labelNames, "the shenyu proxy executor latency millis");
        MetricsReporter.registerHistogram(name2, "the shenyu proxy executor latency millis");
        MetricsReporter.recordTime(name1, labelNames, 1000);
        Map<String, Histogram> histogramMap1 = (Map<String, Histogram>) ReflectUtils.getFieldValue(metricsRegister, "HISTOGRAM_MAP");
        Histogram histogram1 = histogramMap1.get(name1);
        Assertions.assertEquals(histogram1.labels(labelNames).get().sum, 1000.0);
        MetricsReporter.recordTime(name2, 1000);
        Map<String, Histogram> histogramMap2 = (Map<String, Histogram>) ReflectUtils.getFieldValue(metricsRegister, "HISTOGRAM_MAP");
        Histogram histogram2 = histogramMap2.get(name2);
        Assertions.assertEquals(histogram2.labels().get().sum, 1000.0);
    }

    @Test
    public void testZClean() throws Exception {
        String name = "test_metrics_name";
        MetricsReporter.registerCounter(name, DOCUMENT);
        Map<String, Counter> counterMap1 = getPrivateField(metricsRegister, "COUNTER_MAP", Map.class);
        Assertions.assertFalse(CollectionUtils.isEmpty(counterMap1));
        MetricsReporter.clean();
        Map<String, Counter> counterMap2 = getPrivateField(metricsRegister, "COUNTER_MAP", Map.class);
        Assertions.assertTrue(CollectionUtils.isEmpty(counterMap2));
    }

    @AfterAll
    public static void clean() {
        metricsRegister.clean();
        MetricsReporter.clean();
    }
}
