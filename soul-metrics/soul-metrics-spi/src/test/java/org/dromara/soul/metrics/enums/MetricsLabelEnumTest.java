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

package org.dromara.soul.metrics.enums;

import org.junit.Test;

import java.util.Arrays;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 *  The Test For MetricsLabel.
 *
 * @author nuo-promise
 **/
public final class MetricsLabelEnumTest {

    private static final String REQUEST_TOTAL = "request_total";

    private static final String HTTP_REQUEST_TOTAL = "http_request_total";

    private static final String REQUEST_LATENCY = "request_latency";

    private static final String METRICS_LABEL_ENUM_DATA = "[REQUEST_TOTAL, HTTP_REQUEST_TOTAL, REQUEST_LATENCY]";

    @Test
    public void getName() {
        assertThat(MetricsLabelEnum.REQUEST_TOTAL.getName(), is(REQUEST_TOTAL));
        assertThat(MetricsLabelEnum.HTTP_REQUEST_TOTAL.getName(), is(HTTP_REQUEST_TOTAL));
        assertThat(MetricsLabelEnum.REQUEST_LATENCY.getName(), is(REQUEST_LATENCY));
    }

    @Test
    public void values() {
        assertThat(Arrays.toString(MetricsLabelEnum.values()), is(METRICS_LABEL_ENUM_DATA));
    }

    @Test
    public void valueOf() {
        assertThat(MetricsLabelEnum.valueOf("HTTP_REQUEST_TOTAL"), is(MetricsLabelEnum.HTTP_REQUEST_TOTAL));
        assertThat(MetricsLabelEnum.valueOf("REQUEST_TOTAL"), is(MetricsLabelEnum.REQUEST_TOTAL));
        assertThat(MetricsLabelEnum.valueOf("REQUEST_LATENCY"), is(MetricsLabelEnum.REQUEST_LATENCY));
    }
}
