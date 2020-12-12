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

package org.dromara.soul.metrics.prometheus.impl.summary;

import io.prometheus.client.Summary;
import org.dromara.soul.metrics.enums.MetricsLabelEnum;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.concurrent.TimeUnit;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

/**
 * The Test Case For RequestLatencySummaryMetrics.
 *
 * @author nuo-promise
 **/
public final class RequestLatencySummaryMetricsTrackerTest {

    private Summary requestsLatencySummaryMillis;

    @Before
    public void setUp() {
        requestsLatencySummaryMillis = Summary.build()
                .name("summary").help("Requests Latency Summary Millis (ms)")
                .quantile(0.5, 0.05)
                .quantile(0.95, 0.01)
                .quantile(0.99, 0.001)
                .maxAgeSeconds(TimeUnit.MINUTES.toSeconds(5))
                .ageBuckets(5)
                .create();
    }

    @After
    public void tearDown() {
        requestsLatencySummaryMillis.clear();
    }

    @Test(expected = NullPointerException.class)
    public void startTimer() {
        Summary.Timer timer = requestsLatencySummaryMillis.startTimer();
        assertNotNull(timer);
        new PrometheusSummaryMetricsTrackerDelegate(null).observeDuration();
    }

    @Test
    public void metricsLabel() {
        assertThat(MetricsLabelEnum.REQUEST_LATENCY.getName(), is("request_latency"));
    }
}
