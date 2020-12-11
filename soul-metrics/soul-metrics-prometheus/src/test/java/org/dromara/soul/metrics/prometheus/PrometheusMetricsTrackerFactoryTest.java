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

package org.dromara.soul.metrics.prometheus;

import org.dromara.soul.metrics.api.MetricsTracker;
import org.dromara.soul.metrics.prometheus.impl.counter.HttpRequestCounterMetricsTracker;
import org.dromara.soul.metrics.prometheus.impl.counter.RequestTotalCounterMetricsTracker;
import org.dromara.soul.metrics.prometheus.impl.histogram.RequestLatencyHistogramMetricsTracker;
import org.dromara.soul.metrics.prometheus.impl.summary.RequestLatencySummaryMetricsTracker;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collection;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * The Test Case For PrometheusMetricsTracker.
 *
 * @author nuo-promise
 **/
public final class PrometheusMetricsTrackerFactoryTest {

    private static final Collection<MetricsTracker> REGISTER = new ArrayList<>();

    private static final String REQUEST_TOTAL_COUNT_NAME = "request_total";

    private static final String HTTP_REQUEST_TOTAL_NAME = "http_request_total";

    private static final String REQUEST_HISTOGRAM_NAME = "request_latency_histogram_millis";

    private static final String REQUEST_SUMMARY_NAME = "request_latency_summary_millis";

    private static final String REQUEST_TOTAL = "request_total";

    private static final String HTTP_REQUEST_TOTAL = "http_request_total";

    private static final String REQUEST_LATENCY = "request_latency";

    @Before
    public void setUp() {
        REGISTER.add(new RequestTotalCounterMetricsTracker());
        REGISTER.add(new HttpRequestCounterMetricsTracker());
        REGISTER.add(new RequestLatencyHistogramMetricsTracker());
        REGISTER.add(new RequestLatencySummaryMetricsTracker());
    }

    @Test
    public void create() {
        REGISTER.stream().filter(each -> each.metricsLabel().equals(REQUEST_TOTAL) && each.metricsType().equals(REQUEST_TOTAL_COUNT_NAME)).findFirst()
                .ifPresent(metricsTracker -> assertThat(metricsTracker.metricsLabel(), is(REQUEST_TOTAL_COUNT_NAME)));
        REGISTER.stream().filter(each -> each.metricsLabel().equals(HTTP_REQUEST_TOTAL) && each.metricsType().equals(HTTP_REQUEST_TOTAL_NAME)).findFirst()
                .ifPresent(metricsTracker -> assertThat(metricsTracker.metricsLabel(), is(HTTP_REQUEST_TOTAL_NAME)));
        REGISTER.stream().filter(each -> each.metricsLabel().equals(REQUEST_LATENCY) && each.metricsType().equals(REQUEST_HISTOGRAM_NAME)).findFirst()
                .ifPresent(metricsTracker -> assertThat(metricsTracker.metricsLabel(), is(REQUEST_HISTOGRAM_NAME)));
        REGISTER.stream().filter(each -> each.metricsLabel().equals(REQUEST_LATENCY) && each.metricsType().equals(REQUEST_SUMMARY_NAME)).findFirst()
                .ifPresent(metricsTracker -> assertThat(metricsTracker.metricsLabel(), is(REQUEST_SUMMARY_NAME)));

    }
}
