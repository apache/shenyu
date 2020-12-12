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

package org.dromara.soul.metrics.prometheus.impl.counter;

import io.prometheus.client.Counter;
import org.dromara.soul.metrics.enums.MetricsLabelEnum;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * The Test Case For HttpRequestCounter.
 *
 * @author nuo-promise
 **/

public final class HttpRequestCounterMetricsTrackerTest {

    private Counter httpRequestCounter;

    @Before
    public void setUp() {
        httpRequestCounter = Counter.build()
                .name("httpRequestCounter")
                .labelNames("path", "type")
                .help("httpRequestCounter not init")
                .create();
    }

    @After
    public void tearDown() {
        httpRequestCounter.clear();
    }

    @Test
    public void inc() {
        double amt = 1.0D;
        httpRequestCounter.labels("path", "type").inc(amt);
        assertThat(httpRequestCounter.labels("path", "type").get(), is(1.0D));
    }

    @Test
    public void metricsLabel() {
        assertThat(MetricsLabelEnum.HTTP_REQUEST_TOTAL.getName(), is("http_request_total"));
    }
}
