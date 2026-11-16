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

package org.apache.shenyu.plugin.metrics.constant;

import org.apache.shenyu.plugin.api.context.CanaryContext;

import java.util.Arrays;
import java.util.Objects;

/**
 * Shared canary metric definitions for registration and label value ordering.
 */
public enum CanaryMetric {

    REQUESTS("shenyu_canary_requests_total", "External requests attributed to canary routing", null,
            "selector", "rule", "partition", "outcome"),

    FALLBACK("shenyu_canary_fallback_total", "Successful initial fallback from canary to stable", null,
            "selector", "rule", "reason"),

    DECISION_DURATION("shenyu_canary_decision_duration_seconds", "Initial canary decide duration in seconds",
            new double[]{0.000001, 0.000005, 0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1},
            "selector", "rule");

    private final String name;

    private final String description;

    private final double[] buckets;

    private final String[] labelNames;

    CanaryMetric(final String name, final String description, final double[] buckets, final String... labelNames) {
        this.name = name;
        this.description = description;
        this.buckets = buckets;
        this.labelNames = labelNames;
    }

    /**
     * Gets the exported metric name.
     *
     * @return metric name
     */
    public String getName() {
        return name;
    }

    /**
     * Gets the metric description.
     *
     * @return metric description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Gets the label names in registration order.
     *
     * @return a copy of the label names
     */
    public String[] getLabelNames() {
        return labelNames.clone();
    }

    /**
     * Gets the histogram bucket boundaries, or null for a counter.
     *
     * @return a copy of the bucket boundaries, or null
     */
    public double[] getBuckets() {
        return Objects.isNull(buckets) ? null : buckets.clone();
    }

    /**
     * Gets label values in registration order.
     *
     * @param context routing observation
     * @param outcome request outcome, unused by other metrics
     * @return label values
     */
    public String[] getLabelValues(final CanaryContext context, final String outcome) {
        return Arrays.stream(labelNames).map(label -> labelValue(label, context, outcome)).toArray(String[]::new);
    }

    private String labelValue(final String label, final CanaryContext context, final String outcome) {
        switch (label) {
            case "selector":
                return context.getSelectorId();
            case "rule":
                return context.getRuleId();
            case "partition":
                return context.getMetricPartition();
            case "outcome":
                return outcome;
            case "reason":
                return context.getFallbackReason();
            default:
                throw new IllegalArgumentException("Unknown canary metric label: " + label);
        }
    }
}
