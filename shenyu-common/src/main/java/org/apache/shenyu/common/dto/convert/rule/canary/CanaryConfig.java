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

package org.apache.shenyu.common.dto.convert.rule.canary;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.MatchModeEnum;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Canary routing configuration.
 */
public class CanaryConfig {

    /**
     * Whether Canary traffic is enabled. When false, a present configuration routes to Stable.
     */
    private boolean enabled;

    /**
     * Canary matching conditions. Empty conditions allow all requests to participate.
     */
    private List<ConditionData> conditions;

    /**
     * Condition match mode: 0 for AND (default), 1 for OR.
     */
    private Integer matchMode = MatchModeEnum.AND.getCode();

    /**
     * Percentage of eligible identifiers assigned to Canary, from 0 to 100. Defaults to 0.
     */
    private int percentage;

    /**
     * Identifier source for sticky routing. Required when enabled with a percentage from 1 to 99.
     */
    private StickyKeyConfig stickyKey;

    /**
     * Labels used to select canary upstreams.
     */
    private Map<String, String> canaryLabels;

    /**
     * Labels used to select stable upstreams.
     */
    private Map<String, String> stableLabels;

    /**
     * Initial empty Canary pool policy: STABLE (default) or REJECT.
     */
    private String fallbackPolicy = "STABLE";

    /**
     * Get enabled.
     *
     * @return enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Set enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Get conditions.
     *
     * @return conditions
     */
    public List<ConditionData> getConditions() {
        return conditions;
    }

    /**
     * Set conditions.
     *
     * @param conditions conditions
     */
    public void setConditions(final List<ConditionData> conditions) {
        this.conditions = conditions;
    }

    /**
     * Get matchMode.
     *
     * @return matchMode
     */
    public Integer getMatchMode() {
        return matchMode;
    }

    /**
     * Set matchMode.
     *
     * @param matchMode matchMode
     */
    public void setMatchMode(final Integer matchMode) {
        this.matchMode = matchMode;
    }

    /**
     * Get percentage.
     *
     * @return percentage
     */
    public int getPercentage() {
        return percentage;
    }

    /**
     * Set percentage.
     *
     * @param percentage percentage
     */
    public void setPercentage(final int percentage) {
        this.percentage = percentage;
    }

    /**
     * Get stickyKey.
     *
     * @return stickyKey
     */
    public StickyKeyConfig getStickyKey() {
        return stickyKey;
    }

    /**
     * Set stickyKey.
     *
     * @param stickyKey stickyKey
     */
    public void setStickyKey(final StickyKeyConfig stickyKey) {
        this.stickyKey = stickyKey;
    }

    /**
     * Get canaryLabels.
     *
     * @return canaryLabels
     */
    public Map<String, String> getCanaryLabels() {
        return canaryLabels;
    }

    /**
     * Set canaryLabels.
     *
     * @param canaryLabels canaryLabels
     */
    public void setCanaryLabels(final Map<String, String> canaryLabels) {
        this.canaryLabels = canaryLabels;
    }

    /**
     * Get stableLabels.
     *
     * @return stableLabels
     */
    public Map<String, String> getStableLabels() {
        return stableLabels;
    }

    /**
     * Set stableLabels.
     *
     * @param stableLabels stableLabels
     */
    public void setStableLabels(final Map<String, String> stableLabels) {
        this.stableLabels = stableLabels;
    }

    /**
     * Get fallbackPolicy.
     *
     * @return fallbackPolicy
     */
    public String getFallbackPolicy() {
        return fallbackPolicy;
    }

    /**
     * Set fallbackPolicy.
     *
     * @param fallbackPolicy fallbackPolicy
     */
    public void setFallbackPolicy(final String fallbackPolicy) {
        this.fallbackPolicy = fallbackPolicy;
    }

    /**
     * Validate that the two label selectors cannot match the same upstream.
     *
     * @throws IllegalArgumentException when labels are missing or selectors can overlap
     */
    public void validatePartitionLabels() {
        if (Objects.isNull(canaryLabels) || canaryLabels.isEmpty() || Objects.isNull(stableLabels) || stableLabels.isEmpty()) {
            throw new IllegalArgumentException("Canary and Stable labels must both be configured");
        }
        boolean disjoint = canaryLabels.entrySet().stream().anyMatch(entry -> stableLabels.containsKey(entry.getKey())
                && !Objects.equals(entry.getValue(), stableLabels.get(entry.getKey())));
        if (!disjoint) {
            throw new IllegalArgumentException("Canary and Stable labels must have a shared key with different values");
        }
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        CanaryConfig that = (CanaryConfig) o;
        return enabled == that.enabled && percentage == that.percentage
                && Objects.equals(conditions, that.conditions) && Objects.equals(matchMode, that.matchMode)
                && Objects.equals(stickyKey, that.stickyKey) && Objects.equals(canaryLabels, that.canaryLabels)
                && Objects.equals(stableLabels, that.stableLabels) && Objects.equals(fallbackPolicy, that.fallbackPolicy);
    }

    @Override
    public int hashCode() {
        return Objects.hash(enabled, conditions, matchMode, percentage, stickyKey, canaryLabels, stableLabels, fallbackPolicy);
    }

    @Override
    public String toString() {
        return "CanaryConfig{"
                + "enabled=" + enabled
                + ", conditions=" + conditions
                + ", matchMode=" + matchMode
                + ", percentage=" + percentage
                + ", stickyKey=" + stickyKey
                + ", canaryLabels=" + canaryLabels
                + ", stableLabels=" + stableLabels
                + ", fallbackPolicy='" + fallbackPolicy + '\''
                + '}';
    }
}
