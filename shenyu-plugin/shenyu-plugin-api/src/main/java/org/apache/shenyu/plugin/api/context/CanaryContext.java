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

package org.apache.shenyu.plugin.api.context;

import java.util.Objects;

/**
 * Request-scoped observation populated by Divide during the first canary selection.
 * Retry routing continues to use its existing exchange attributes.
 */
public final class CanaryContext {

    public static final String CANARY_POOL_EMPTY = "canary_pool_empty";

    public static final String STABLE_POOL_EMPTY = "stable_pool_empty";

    public static final String NO_UPSTREAM_SELECTED = "no_upstream_selected";

    private final String selectorId;

    private final String ruleId;

    private final String intendedPartition;

    private String actualPartition;

    private String fallbackReason;

    private String rejectReason;

    private final long decisionDurationNanos;

    /**
     * Creates a routing observation. An unsuccessful selection has no actual partition.
     *
     * @param selectorId selector configuration ID
     * @param ruleId rule configuration ID
     * @param intendedPartition original decision: stable or canary
     * @param actualPartition selected partition, or null if no upstream was selected
     * @param fallbackReason successful fallback reason, or null
     * @param rejectReason routing rejection reason, or null
     * @param decisionDurationNanos duration of the initial decide call
     */
    public CanaryContext(final String selectorId, final String ruleId, final String intendedPartition,
                         final String actualPartition, final String fallbackReason, final String rejectReason,
                         final long decisionDurationNanos) {
        this.selectorId = selectorId;
        this.ruleId = ruleId;
        this.intendedPartition = intendedPartition;
        this.actualPartition = actualPartition;
        this.fallbackReason = fallbackReason;
        this.rejectReason = rejectReason;
        this.decisionDurationNanos = decisionDurationNanos;
    }

    /**
     * Gets the selector configuration ID.
     *
     * @return the selector configuration ID
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * Gets the rule configuration ID.
     *
     * @return the rule configuration ID
     */
    public String getRuleId() {
        return ruleId;
    }

    /**
     * Gets the originally intended partition.
     *
     * @return the originally intended partition
     */
    public String getIntendedPartition() {
        return intendedPartition;
    }

    /**
     * Gets the selected partition, or null.
     *
     * @return the selected partition, or null
     */
    public String getActualPartition() {
        return actualPartition;
    }

    /**
     * Sets the selected partition, or null.
     *
     * @param actualPartition the selected partition, or null
     */
    public void setActualPartition(final String actualPartition) {
        this.actualPartition = actualPartition;
    }

    /**
     * Gets the successful fallback reason, or null.
     *
     * @return the successful fallback reason, or null
     */
    public String getFallbackReason() {
        return fallbackReason;
    }

    /**
     * Sets the successful fallback reason, or null.
     *
     * @param fallbackReason the successful fallback reason, or null
     */
    public void setFallbackReason(final String fallbackReason) {
        this.fallbackReason = fallbackReason;
    }

    /**
     * Gets the rejection reason, or null.
     *
     * @return the rejection reason, or null
     */
    public String getRejectReason() {
        return rejectReason;
    }

    /**
     * Sets the rejection reason, or null.
     *
     * @param rejectReason the rejection reason, or null
     */
    public void setRejectReason(final String rejectReason) {
        this.rejectReason = rejectReason;
    }

    /**
     * Gets the initial decision duration in nanoseconds.
     *
     * @return the initial decision duration in nanoseconds
     */
    public long getDecisionDurationNanos() {
        return decisionDurationNanos;
    }

    /**
     * Gets the metric partition, attributing unselected requests to their original intent.
     *
     * @return stable or canary
     */
    public String getMetricPartition() {
        return Objects.isNull(actualPartition) ? intendedPartition : actualPartition;
    }
}
