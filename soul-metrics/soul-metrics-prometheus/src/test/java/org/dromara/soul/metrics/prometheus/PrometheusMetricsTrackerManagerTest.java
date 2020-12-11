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

import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * The Test Case For PrometheusMetricsTrackerManager.
 *
 * @author nuo-promise
 **/
public final class PrometheusMetricsTrackerManagerTest {

    private static final String REQUEST_TOTAL_COUNT_NAME = "request_total";

    private static final String REQUEST_TOTAL = "request_total";

    @Test
    public void getMetricsTrackerFactory() {
        new PrometheusMetricsTrackerFactory().create(REQUEST_TOTAL_COUNT_NAME, REQUEST_TOTAL)
                .ifPresent(metricsTracker -> assertThat(metricsTracker.metricsLabel(), is(REQUEST_TOTAL_COUNT_NAME)));
    }
}
