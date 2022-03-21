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

package org.apache.shenyu.metrics.reporter;

import org.apache.shenyu.metrics.entity.Metric;
import org.apache.shenyu.metrics.enums.MetricType;
import org.apache.shenyu.metrics.spi.MetricsRegister;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

/**
 * MetricsReporterTest.
 */
public class MetricsReporterTest {

    private static MetricsRegister metricsRegister;

    @Test
    public void testRegister() {
        MetricsReporter.register(metricsRegister);
    }

    @Test
    public void testRegisterMetrics() {
        Assertions.assertThrows(RuntimeException.class, () -> {
            List<String> labels = Collections.singletonList("label");
            Collection<Metric> metricsCounter = Collections.singletonList(new Metric(MetricType.COUNTER,
                    "request_total", "shenyu request total count", labels));
            MetricsReporter.registerMetrics(metricsCounter);
            Collection<Metric> metricsGauge = Collections.singletonList(new Metric(MetricType.GAUGE,
                    "jvm.total.used", "shenyu jvm total used", labels));
            MetricsReporter.registerMetrics(metricsGauge);
            Collection<Metric> metricsHistogram = Collections.singletonList(new Metric(MetricType.HISTOGRAM,
                    "requests_latency_histogram_millis", "the shenyu proxy executor latency millis", labels));
            MetricsReporter.registerMetrics(metricsHistogram);
            Collection<Metric> metricsNull = Collections.singletonList(new Metric(null,
                    null, null, labels));
            MetricsReporter.registerMetrics(metricsNull);
        });
    }

    @Test
    public void testRegisterCounter() {
        MetricsReporter.registerCounter("pending.jobs.size", new String[]{"label"}, "shenyu pending jobs size");
        MetricsReporter.registerCounter("pending.jobs.size", "shenyu pending jobs size");
    }

    @Test
    public void testRegisterGauge() {
        MetricsReporter.registerGauge("jvm.total.used", new String[]{"label"}, "shenyu jvm total used");
        MetricsReporter.registerGauge("jvm.total.used", "shenyu jvm total used");
    }

    @Test
    public void testHistogram() {
        MetricsReporter.registerHistogram("request.histogram", new String[]{"label"}, "shenyu request total");
        MetricsReporter.registerHistogram("request.histogram", "shenyu request total");
    }

    @Test
    public void testCounterIncrement() {
        MetricsReporter.counterIncrement("requestCountIncrement", new String[]{"label"});
        MetricsReporter.counterIncrement("requestCountIncrement");
        MetricsReporter.counterIncrement("requestCountIncrement", new String[]{"label"}, 1);
    }

    @Test
    public void testGaugeIncrement() {
        MetricsReporter.gaugeIncrement("requestGaugeIncrement", new String[]{"label"});
        MetricsReporter.gaugeIncrement("requestGaugeIncrement");
    }

    @Test
    public void testGaugeDecrement() {
        MetricsReporter.gaugeDecrement("gaugeDecrement", new String[]{"label"});
        MetricsReporter.gaugeDecrement("gaugeDecrement");
    }

    @Test
    public void testRecordTime() {
        MetricsReporter.recordTime("executeTime", new String[]{"label"}, 1L);
        MetricsReporter.recordTime("executeTime", 1L);
    }
}
