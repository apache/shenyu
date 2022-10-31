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

package org.apache.shenyu.common.dto.convert.rule;

import org.apache.shenyu.common.constant.Constants;

import java.util.Objects;

/**
 * This is SentinelHandle.
 */
public class SentinelHandle {

    /**
     * Flow rule enable.
     */
    private Integer flowRuleEnable = Constants.SENTINEL_ENABLE_FLOW_RULE;

    /**
     * Flow rule grade.
     */
    private Integer flowRuleGrade = Constants.SENTINEL_QPS_FLOW_GRADE;

    /**
     * Max queueing time in rate limiter behavior (ms).
     */
    private int flowRuleMaxQueueingTimeMs = 500;


    /**
     * The flow control warm-up time (s).
     */
    private int flowRuleWarmUpPeriodSec = 10;

    /**
     * Flow rule count.
     */
    private Integer flowRuleCount;

    /**
     * Flow rule control behavior.
     */
    private Integer flowRuleControlBehavior = Constants.SENTINEL_FLOW_REJECT;

    /**
     * Degrade rule control behavior.
     */
    private Integer degradeRuleEnable = Constants.SENTINEL_ENABLE_DEGRADE_RULE;

    /**
     * Degrade rule grade.
     */
    private Integer degradeRuleGrade = Constants.SENTINEL_RESPONSE_RULE_GRADE;

    /**
     * Degrade rule count.
     */
    private Double degradeRuleCount;

    /**
     * Degrade rule time window.
     */
    private Integer degradeRuleTimeWindow;

    /**
     * Degrade rule min request amount.
     */
    private Integer degradeRuleMinRequestAmount = Constants.SENTINEL_MIN_REQUEST_AMOUNT;

    /**
     * Degrade rule slow ratio threshold.
     */
    private Double degradeRuleSlowRatioThreshold = Constants.SENTINEL_SLOW_RATIO_THRESHOLD;

    /**
     * Degrade rule stat intervalMs.
     */
    private int degradeRuleStatIntervals = Constants.SENTINEL_STAT_INTERVALS;

    /**
     * Sentinel fallback uri.
     */
    private String fallbackUri;

    /**
     * get flowRuleEnable.
     *
     * @return flowRuleEnable
     */
    public Integer getFlowRuleEnable() {
        return flowRuleEnable;
    }

    /**
     * set flowRuleEnable.
     *
     * @param flowRuleEnable flowRuleEnable
     */
    public void setFlowRuleEnable(final Integer flowRuleEnable) {
        this.flowRuleEnable = flowRuleEnable;
    }

    /**
     * get flowRuleGrade.
     *
     * @return flowRuleGrade
     */
    public Integer getFlowRuleGrade() {
        return flowRuleGrade;
    }

    /**
     * set flowRuleGrade.
     *
     * @param flowRuleGrade flowRuleGrade
     */
    public void setFlowRuleGrade(final Integer flowRuleGrade) {
        this.flowRuleGrade = flowRuleGrade;
    }

    /**
     * get flowRuleCount.
     *
     * @return flowRuleCount
     */
    public Integer getFlowRuleCount() {
        return flowRuleCount;
    }

    /**
     * set flowRuleCount.
     *
     * @param flowRuleCount flowRuleCount
     */
    public void setFlowRuleCount(final Integer flowRuleCount) {
        this.flowRuleCount = flowRuleCount;
    }

    /**
     * get flowRuleControlBehavior.
     *
     * @return flowRuleControlBehavior
     */
    public Integer getFlowRuleControlBehavior() {
        return flowRuleControlBehavior;
    }

    /**
     * set flowRuleControlBehavior.
     *
     * @param flowRuleControlBehavior flowRuleControlBehavior
     */
    public void setFlowRuleControlBehavior(final Integer flowRuleControlBehavior) {
        this.flowRuleControlBehavior = flowRuleControlBehavior;
    }

    /**
     * get degradeRuleEnable.
     *
     * @return degradeRuleEnable
     */
    public Integer getDegradeRuleEnable() {
        return degradeRuleEnable;
    }

    /**
     * set degradeRuleEnable.
     *
     * @param degradeRuleEnable degradeRuleEnable
     */
    public void setDegradeRuleEnable(final Integer degradeRuleEnable) {
        this.degradeRuleEnable = degradeRuleEnable;
    }

    /**
     * get degradeRuleGrade.
     *
     * @return degradeRuleGrade
     */
    public Integer getDegradeRuleGrade() {
        return degradeRuleGrade;
    }

    /**
     * set degradeRuleGrade.
     *
     * @param degradeRuleGrade degradeRuleGrade
     */
    public void setDegradeRuleGrade(final Integer degradeRuleGrade) {
        this.degradeRuleGrade = degradeRuleGrade;
    }

    /**
     * get degradeRuleCount.
     *
     * @return degradeRuleCount
     */
    public Double getDegradeRuleCount() {
        return degradeRuleCount;
    }

    /**
     * set degradeRuleCount.
     *
     * @param degradeRuleCount degradeRuleCount
     */
    public void setDegradeRuleCount(final Double degradeRuleCount) {
        this.degradeRuleCount = degradeRuleCount;
    }

    /**
     * get degradeRuleTimeWindow.
     *
     * @return degradeRuleTimeWindow
     */
    public Integer getDegradeRuleTimeWindow() {
        return degradeRuleTimeWindow;
    }

    /**
     * set degradeRuleTimeWindow.
     *
     * @param degradeRuleTimeWindow degradeRuleTimeWindow
     */
    public void setDegradeRuleTimeWindow(final Integer degradeRuleTimeWindow) {
        this.degradeRuleTimeWindow = degradeRuleTimeWindow;
    }

    /**
     * get degradeRuleMinRequestAmount.
     *
     * @return degradeRuleMinRequestAmount
     */
    public Integer getDegradeRuleMinRequestAmount() {
        return degradeRuleMinRequestAmount;
    }

    /**
     * set degradeRuleMinRequestAmount.
     *
     * @param degradeRuleMinRequestAmount degradeRuleMinRequestAmount
     */
    public void setDegradeRuleMinRequestAmount(final Integer degradeRuleMinRequestAmount) {
        this.degradeRuleMinRequestAmount = degradeRuleMinRequestAmount;
    }

    /**
     * get degradeRuleSlowRatioThreshold.
     *
     * @return degradeRuleSlowRatioThreshold
     */
    public Double getDegradeRuleSlowRatioThreshold() {
        return degradeRuleSlowRatioThreshold;
    }

    /**
     * set degradeRuleSlowRatioThreshold.
     *
     * @param degradeRuleSlowRatioThreshold degradeRuleSlowRatioThreshold
     */
    public void setDegradeRuleSlowRatioThreshold(final Double degradeRuleSlowRatioThreshold) {
        this.degradeRuleSlowRatioThreshold = degradeRuleSlowRatioThreshold;
    }

    /**
     * get degradeRuleStatIntervals.
     *
     * @return degradeRuleStatIntervals
     */
    public int getDegradeRuleStatIntervals() {
        return degradeRuleStatIntervals;
    }

    /**
     * set degradeRuleStatIntervals.
     *
     * @param degradeRuleStatIntervals degradeRuleStatIntervals
     */
    public void setDegradeRuleStatIntervals(final int degradeRuleStatIntervals) {
        this.degradeRuleStatIntervals = degradeRuleStatIntervals;
    }

    /**
     * get fallbackUri.
     *
     * @return fallbackUri
     */
    public String getFallbackUri() {
        return fallbackUri;
    }

    /**
     * set fallbackUri.
     *
     * @param fallbackUri fallbackUri
     */
    public void setFallbackUri(final String fallbackUri) {
        this.fallbackUri = fallbackUri;
    }

    /**
     * flowRuleMaxQueueingTimeMs.
     *
     * @return FlowRuleMaxQueueingTimeMs
     */
    public int getFlowRuleMaxQueueingTimeMs() {
        return flowRuleMaxQueueingTimeMs;
    }

    /**
     * set flowRuleMaxQueueingTimeMs.
     *
     * @param flowRuleMaxQueueingTimeMs flowRuleMaxQueueingTimeMs
     */
    public void setFlowRuleMaxQueueingTimeMs(final int flowRuleMaxQueueingTimeMs) {
        this.flowRuleMaxQueueingTimeMs = flowRuleMaxQueueingTimeMs;
    }

    /**
     * flowRuleWarmUpPeriodSec.
     *
     * @return FlowRuleWarmUpPeriodSec
     */
    public int getFlowRuleWarmUpPeriodSec() {
        return flowRuleWarmUpPeriodSec;
    }

    /**
     * set flowRuleWarmUpPeriodSec.
     *
     * @param flowRuleWarmUpPeriodSec flowRuleWarmUpPeriodSec
     */
    public void setFlowRuleWarmUpPeriodSec(final int flowRuleWarmUpPeriodSec) {
        this.flowRuleWarmUpPeriodSec = flowRuleWarmUpPeriodSec;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        SentinelHandle that = (SentinelHandle) o;
        return degradeRuleStatIntervals == that.degradeRuleStatIntervals && Objects.equals(flowRuleEnable, that.flowRuleEnable)
                && Objects.equals(flowRuleGrade, that.flowRuleGrade) && Objects.equals(flowRuleCount, that.flowRuleCount)
                && Objects.equals(flowRuleControlBehavior, that.flowRuleControlBehavior) && Objects.equals(degradeRuleEnable, that.degradeRuleEnable)
                && Objects.equals(degradeRuleGrade, that.degradeRuleGrade) && Objects.equals(degradeRuleCount, that.degradeRuleCount)
                && Objects.equals(degradeRuleTimeWindow, that.degradeRuleTimeWindow) && Objects.equals(degradeRuleMinRequestAmount, that.degradeRuleMinRequestAmount)
                && Objects.equals(degradeRuleSlowRatioThreshold, that.degradeRuleSlowRatioThreshold) && Objects.equals(fallbackUri, that.fallbackUri)
                && Objects.equals(flowRuleMaxQueueingTimeMs, that.flowRuleMaxQueueingTimeMs) && Objects.equals(flowRuleWarmUpPeriodSec, that.flowRuleWarmUpPeriodSec);
    }

    @Override
    public int hashCode() {
        return Objects.hash(flowRuleEnable, flowRuleGrade, flowRuleCount, flowRuleControlBehavior, degradeRuleEnable, degradeRuleGrade,
                degradeRuleCount, degradeRuleTimeWindow, degradeRuleMinRequestAmount, degradeRuleSlowRatioThreshold, degradeRuleStatIntervals,
                fallbackUri, flowRuleMaxQueueingTimeMs, flowRuleWarmUpPeriodSec);
    }

    @Override
    public String toString() {
        return "SentinelHandle{"
                + "flowRuleEnable="
                + flowRuleEnable
                + ", flowRuleGrade="
                + flowRuleGrade
                + ", flowRuleCount="
                + flowRuleCount
                + ", flowRuleControlBehavior="
                + flowRuleControlBehavior
                + ", degradeRuleEnable="
                + degradeRuleEnable
                + ", degradeRuleGrade="
                + degradeRuleGrade
                + ", degradeRuleCount="
                + degradeRuleCount
                + ", degradeRuleTimeWindow="
                + degradeRuleTimeWindow
                + ", degradeRuleMinRequestAmount="
                + degradeRuleMinRequestAmount
                + ", degradeRuleSlowRatioThreshold="
                + degradeRuleSlowRatioThreshold
                + ", degradeRuleStatIntervals="
                + degradeRuleStatIntervals
                + ", fallbackUri='"
                + fallbackUri
                + ", flowRuleMaxQueueingTimeMs='"
                + flowRuleMaxQueueingTimeMs
                + ", flowRuleWarmUpPeriodSec='"
                + flowRuleWarmUpPeriodSec
                + '\''
                + '}';
    }

    /**
     * check filed default value.
     *
     * @param sentinelHandle {@linkplain SentinelHandle}
     */
    public void checkData(final SentinelHandle sentinelHandle) {
        sentinelHandle.setFlowRuleEnable((sentinelHandle.getFlowRuleEnable() == 1 || sentinelHandle.getFlowRuleEnable() == 0)
                ? sentinelHandle.getFlowRuleEnable() : Constants.SENTINEL_ENABLE_FLOW_RULE);
        sentinelHandle.setDegradeRuleEnable((sentinelHandle.getDegradeRuleEnable() == 1 || sentinelHandle.getDegradeRuleEnable() == 0)
                ? sentinelHandle.getDegradeRuleEnable() : Constants.SENTINEL_ENABLE_DEGRADE_RULE);
    }
}
