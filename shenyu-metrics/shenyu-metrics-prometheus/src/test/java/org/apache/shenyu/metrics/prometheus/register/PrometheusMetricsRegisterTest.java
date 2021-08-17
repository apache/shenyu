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

package org.apache.shenyu.metrics.prometheus.register;

import io.prometheus.client.Counter;
import io.prometheus.client.Histogram;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.junit.Test;

import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

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
        assertThat(histogramMap.size(), is(2));
        Histogram histogram = histogramMap.get(name);
        assertThat(histogram.labels(labelNames).get().sum, is(1000.0));
    }
}
