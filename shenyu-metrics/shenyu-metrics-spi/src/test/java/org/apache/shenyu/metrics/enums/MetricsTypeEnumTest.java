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

package org.apache.shenyu.metrics.enums;

import java.util.Arrays;

import org.junit.jupiter.api.Test;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * The Test Case For MetricsTypeEnum.
 */
public final class MetricsTypeEnumTest {

    private static final String METRICS_TYPE_ENUM_DATA = "[COUNTER, GAUGE, HISTOGRAM]";

    @Test
    public void values() {
        assertThat(Arrays.toString(MetricType.values()), is(METRICS_TYPE_ENUM_DATA));
    }

    @Test
    public void valueOf() {
        assertThat(MetricType.valueOf("COUNTER"), is(MetricType.COUNTER));
        assertThat(MetricType.valueOf("GAUGE"), is(MetricType.GAUGE));
        assertThat(MetricType.valueOf("HISTOGRAM"), is(MetricType.HISTOGRAM));
    }
}
