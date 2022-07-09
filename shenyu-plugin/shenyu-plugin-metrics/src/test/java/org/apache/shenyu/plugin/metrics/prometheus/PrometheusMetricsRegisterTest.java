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

package org.apache.shenyu.plugin.metrics.prometheus;

import io.prometheus.client.Counter;
import io.prometheus.client.Gauge;
import io.prometheus.client.Histogram;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.lang.reflect.Field;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public final class PrometheusMetricsRegisterTest {

    private final PrometheusMetricsRegister prometheusMetricsRegister = new PrometheusMetricsRegister();

    @Test
    @SuppressWarnings("unchecked")
    public void assertCounter() {
        String requestTotal = "request_total";
        String[] labelNames = new String[] {"name"};
        prometheusMetricsRegister.registerCounter(requestTotal, labelNames, "shenyu request total count");
        prometheusMetricsRegister.counterIncrement(requestTotal, labelNames, 1);
        prometheusMetricsRegister.counterIncrement(requestTotal, labelNames, 2);
        Map<String, Counter> counterMap = (Map<String, Counter>) ReflectUtils.getFieldValue(prometheusMetricsRegister, "COUNTER_MAP");
        assertNotNull(counterMap);
        assertThat(counterMap.size(), is(1));
        Counter routeDatasourceCounter = counterMap.get(requestTotal);
        assertThat(routeDatasourceCounter.labels(labelNames).get(), is(3.0d));
    }

    @Test
    @SuppressWarnings("unchecked")
    public void assertHistogram() {
        String name = "requests_latency_histogram_millis";
        String[] labelNames = new String[] {"name"};
        prometheusMetricsRegister.registerHistogram(name, labelNames, "the shenyu proxy executor latency millis");
        prometheusMetricsRegister.recordTime(name, labelNames, 1000);
        String latencyMillis = "execute_latency_millis";
        prometheusMetricsRegister.registerHistogram(latencyMillis, null, "the shenyu executor latency millis");
        prometheusMetricsRegister.recordTime(latencyMillis, null, 1000);
        Map<String, Histogram> histogramMap = (Map<String, Histogram>) ReflectUtils.getFieldValue(prometheusMetricsRegister, "HISTOGRAM_MAP");
        assertNotNull(histogramMap);
        assertThat(histogramMap.size(), is(2));
        Histogram histogram = histogramMap.get(name);
        assertThat(histogram.labels(labelNames).get().sum, is(1000.0));
    }

    @Test
    public void testRegisterGauge() throws Exception {
        String name = "request_throw_total";
        String[] labelNames = new String[] {"name1", "name2"};
        prometheusMetricsRegister.registerGauge(name, labelNames, "shenyu request total count");
        Field field = prometheusMetricsRegister.getClass().getDeclaredField("GAUGE_MAP");
        field.setAccessible(true);
        Map<String, Gauge> map = (Map<String, Gauge>) field.get(prometheusMetricsRegister);
        Assertions.assertEquals(map.get(name).describe().toString(),
                "[Name: request_throw_total Unit: Type: GAUGE Help: shenyu request total count Samples: []]");
        Gauge gauge = map.get(name);
        prometheusMetricsRegister.gaugeIncrement(name, labelNames);
        Assertions.assertEquals(gauge.labels(labelNames).get(), 1.0);
        prometheusMetricsRegister.gaugeDecrement(name, labelNames);
        Assertions.assertEquals(gauge.labels(labelNames).get(), 0.0);
        prometheusMetricsRegister.clean();
    }

    @Test
    public void testClean() throws Exception {
        prometheusMetricsRegister.clean();
        Field field1 = prometheusMetricsRegister.getClass().getDeclaredField("COUNTER_MAP");
        field1.setAccessible(true);
        Map<String, Counter> map1 = (Map<String, Counter>) field1.get(prometheusMetricsRegister);
        Assertions.assertTrue(CollectionUtils.isEmpty(map1));
        Field field2 = prometheusMetricsRegister.getClass().getDeclaredField("GAUGE_MAP");
        field2.setAccessible(true);
        Map<String, Gauge> map2 = (Map<String, Gauge>) field2.get(prometheusMetricsRegister);
        Assertions.assertTrue(CollectionUtils.isEmpty(map2));
        Field field3 = prometheusMetricsRegister.getClass().getDeclaredField("HISTOGRAM_MAP");
        field3.setAccessible(true);
        Map<String, Histogram> map3 = (Map<String, Histogram>) field3.get(prometheusMetricsRegister);
        Assertions.assertTrue(CollectionUtils.isEmpty(map3));
    }
}
