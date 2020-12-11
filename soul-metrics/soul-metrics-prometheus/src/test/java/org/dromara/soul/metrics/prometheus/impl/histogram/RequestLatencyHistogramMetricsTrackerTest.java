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

package org.dromara.soul.metrics.prometheus.impl.histogram;

import io.prometheus.client.Histogram;
import org.dromara.soul.metrics.enums.MetricsLabelEnum;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

/**
 * The Test Case For RequestLatencyHistogram.
 *
 * @author nuo-promise
 **/
public final class RequestLatencyHistogramMetricsTrackerTest {

    private Histogram requestLatencyHistogram;

    @Before
    public void setUp() {
        requestLatencyHistogram = Histogram.build()
                .name("histogram").help("Requests Latency Histogram Millis (ms)")
                .create();
    }

    @After
    public void tearDown() {
        requestLatencyHistogram.clear();
    }

    @Test(expected = NullPointerException.class)
    public void startTimer() {
        Histogram.Timer timer = requestLatencyHistogram.startTimer();
        assertNotNull(timer);
        new PrometheusHistogramMetricsTrackerDelegate(null).observeDuration();
    }

    @Test
    public void metricsLabel() {
        assertThat(MetricsLabelEnum.REQUEST_LATENCY.getName(), is("request_latency"));
    }
}
