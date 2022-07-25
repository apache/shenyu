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

package org.apache.shenyu.plugin.metrics.config;

import org.apache.shenyu.plugin.metrics.enums.MetricType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * The Test Case For Metric.
 */
public final class MetricTest {

    private static final String NAME = "testName";

    private static final String DOCUMENT = "testDocument";

    private List<String> labels;

    private Metric metric;

    @Test
    public void testMetric() {
        labels = new ArrayList<>();
        labels.add("shenyu_request_total");
        metric = new Metric(MetricType.COUNTER, NAME, DOCUMENT, labels);
        Assertions.assertEquals(metric.getType(), MetricType.COUNTER);
        Assertions.assertEquals(metric.getName(), "testName");
        Assertions.assertEquals(metric.getDocument(), "testDocument");
        Assertions.assertEquals(metric.getLabels().get(0), "shenyu_request_total");
    }
}
