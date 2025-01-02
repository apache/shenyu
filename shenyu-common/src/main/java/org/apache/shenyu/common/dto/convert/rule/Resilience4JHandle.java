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
 * this is Resilience4J plugin handle.
 */
public class Resilience4JHandle {

    /**
     * ratelimiter timeoutDurationRate.
     */
    private int timeoutDurationRate = Constants.TIMEOUT_DURATION_RATE;

    /**
     * ratelimiter limitRefreshPeriod.
     */
    private int limitRefreshPeriod = Constants.LIMIT_REFRESH_PERIOD;

    /**
     * ratelimiter limitForPeriod.
     */
    private int limitForPeriod = Constants.LIMIT_FOR_PERIOD;

    /**
     * circuitBreaker circuitEnable.
     */
    private int circuitEnable = Constants.CIRCUIT_DISABLE;

    /**
     * circuitBreaker timeoutDuration.
     */
    private long timeoutDuration = Constants.TIMEOUT_DURATION;

    /**
     * circuitBreaker fallbackUri.
     */
    private String fallbackUri = "/fallback/resilience4j";

    /**
     * circuitBreaker slidingWindowSize.
     */
    private int slidingWindowSize = Constants.SLIDING_WINDOW_SIZE;

    /**
     * circuitBreaker slidingWindowType.
     */
    private int slidingWindowType = Constants.SLIDING_WINDOW_TYPE;

    /**
     * circuitBreaker minimumNumberOfCalls.
     */
    private int minimumNumberOfCalls = Constants.MINIMUM_NUMBER_OF_CALLS;

    /**
     * circuitBreaker waitIntervalFunctionInOpenState.
     */
    private int waitIntervalFunctionInOpenState = Constants.WAIT_INTERVAL_FUNCTION_IN_OPEN_STATE;

    /**
     * circuitBreaker permittedNumberOfCallsInHalfOpenState.
     */
    private int permittedNumberOfCallsInHalfOpenState = Constants.PERMITTED_NUMBER_OF_CALLS_IN_HALF_OPEN_STATE;

    /**
     * circuitBreaker failureRateThreshold.
     */
    private float failureRateThreshold = Constants.FAILURE_RATE_THRESHOLD;

    /**
     * circuitBreaker automaticTransitionFromOpenToHalfOpenEnabled.
     */
    private Boolean automaticTransitionFromOpenToHalfOpenEnabled = Constants.AUTOMATIC_TRANSITION_FROM_OPEN_TO_HALF_OPEN_ENABLED;
    
    /**
     * New default instance resilience 4 j handle.
     *
     * @return the resilience 4 j handle
     */
    public static Resilience4JHandle newDefaultInstance() {
        return new Resilience4JHandle();
    }
    
    /**
     * get timeoutDurationRate.
     *
     * @return timeoutDurationRate timeout duration rate
     */
    public int getTimeoutDurationRate() {
        return timeoutDurationRate;
    }
    
    /**
     * set timeoutDurationRate.
     *
     * @param timeoutDurationRate timeoutDurationRate
     */
    public void setTimeoutDurationRate(final int timeoutDurationRate) {
        this.timeoutDurationRate = timeoutDurationRate;
    }
    
    /**
     * get limitRefreshPeriod.
     *
     * @return limitRefreshPeriod limit refresh period
     */
    public int getLimitRefreshPeriod() {
        return limitRefreshPeriod;
    }
    
    /**
     * set limitRefreshPeriod.
     *
     * @param limitRefreshPeriod limitRefreshPeriod
     */
    public void setLimitRefreshPeriod(final int limitRefreshPeriod) {
        this.limitRefreshPeriod = limitRefreshPeriod;
    }
    
    /**
     * get limitForPeriod.
     *
     * @return limitForPeriod limit for period
     */
    public int getLimitForPeriod() {
        return limitForPeriod;
    }
    
    /**
     * set limitForPeriod.
     *
     * @param limitForPeriod limitForPeriod
     */
    public void setLimitForPeriod(final int limitForPeriod) {
        this.limitForPeriod = limitForPeriod;
    }
    
    /**
     * get circuitEnable.
     *
     * @return circuitEnable circuit enable
     */
    public int getCircuitEnable() {
        return circuitEnable;
    }
    
    /**
     * set circuitEnable.
     *
     * @param circuitEnable circuitEnable
     */
    public void setCircuitEnable(final int circuitEnable) {
        this.circuitEnable = circuitEnable;
    }
    
    /**
     * get timeoutDuration.
     *
     * @return timeoutDuration timeout duration
     */
    public long getTimeoutDuration() {
        return timeoutDuration;
    }
    
    /**
     * set timeoutDuration.
     *
     * @param timeoutDuration timeoutDuration
     */
    public void setTimeoutDuration(final long timeoutDuration) {
        this.timeoutDuration = timeoutDuration;
    }
    
    /**
     * get fallbackUri.
     *
     * @return fallbackUri fallback uri
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
     * get slidingWindowSize.
     *
     * @return slidingWindowSize sliding window size
     */
    public int getSlidingWindowSize() {
        return slidingWindowSize;
    }
    
    /**
     * set slidingWindowSize.
     *
     * @param slidingWindowSize slidingWindowSize
     */
    public void setSlidingWindowSize(final int slidingWindowSize) {
        this.slidingWindowSize = slidingWindowSize;
    }
    
    /**
     * get slidingWindowType.
     *
     * @return slidingWindowType sliding window type
     */
    public int getSlidingWindowType() {
        return slidingWindowType;
    }
    
    /**
     * set slidingWindowType.
     *
     * @param slidingWindowType slidingWindowType
     */
    public void setSlidingWindowType(final int slidingWindowType) {
        this.slidingWindowType = slidingWindowType;
    }
    
    /**
     * get minimumNumberOfCalls.
     *
     * @return minimumNumberOfCalls minimum number of calls
     */
    public int getMinimumNumberOfCalls() {
        return minimumNumberOfCalls;
    }
    
    /**
     * set minimumNumberOfCalls.
     *
     * @param minimumNumberOfCalls minimumNumberOfCalls
     */
    public void setMinimumNumberOfCalls(final int minimumNumberOfCalls) {
        this.minimumNumberOfCalls = minimumNumberOfCalls;
    }
    
    /**
     * get waitIntervalFunctionInOpenState.
     *
     * @return waitIntervalFunctionInOpenState wait interval function in open state
     */
    public int getWaitIntervalFunctionInOpenState() {
        return waitIntervalFunctionInOpenState;
    }
    
    /**
     * set waitIntervalFunctionInOpenState.
     *
     * @param waitIntervalFunctionInOpenState waitIntervalFunctionInOpenState
     */
    public void setWaitIntervalFunctionInOpenState(final int waitIntervalFunctionInOpenState) {
        this.waitIntervalFunctionInOpenState = waitIntervalFunctionInOpenState;
    }
    
    /**
     * get permittedNumberOfCallsInHalfOpenState.
     *
     * @return permittedNumberOfCallsInHalfOpenState permitted number of calls in half open state
     */
    public int getPermittedNumberOfCallsInHalfOpenState() {
        return permittedNumberOfCallsInHalfOpenState;
    }
    
    /**
     * set permittedNumberOfCallsInHalfOpenState.
     *
     * @param permittedNumberOfCallsInHalfOpenState permittedNumberOfCallsInHalfOpenState
     */
    public void setPermittedNumberOfCallsInHalfOpenState(final int permittedNumberOfCallsInHalfOpenState) {
        this.permittedNumberOfCallsInHalfOpenState = permittedNumberOfCallsInHalfOpenState;
    }
    
    /**
     * get failureRateThreshold.
     *
     * @return failureRateThreshold failure rate threshold
     */
    public float getFailureRateThreshold() {
        return failureRateThreshold;
    }
    
    /**
     * set failureRateThreshold.
     *
     * @param failureRateThreshold failureRateThreshold
     */
    public void setFailureRateThreshold(final float failureRateThreshold) {
        this.failureRateThreshold = failureRateThreshold;
    }
    
    /**
     * get automaticTransitionFromOpenToHalfOpenEnabled.
     *
     * @return automaticTransitionFromOpenToHalfOpenEnabled automatic transition from open to half open enabled
     */
    public Boolean getAutomaticTransitionFromOpenToHalfOpenEnabled() {
        return automaticTransitionFromOpenToHalfOpenEnabled;
    }
    
    /**
     * set automaticTransitionFromOpenToHalfOpenEnabled.
     *
     * @param automaticTransitionFromOpenToHalfOpenEnabled automaticTransitionFromOpenToHalfOpenEnabled
     */
    public void setAutomaticTransitionFromOpenToHalfOpenEnabled(final Boolean automaticTransitionFromOpenToHalfOpenEnabled) {
        this.automaticTransitionFromOpenToHalfOpenEnabled = automaticTransitionFromOpenToHalfOpenEnabled;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        Resilience4JHandle that = (Resilience4JHandle) o;
        return timeoutDurationRate == that.timeoutDurationRate && limitRefreshPeriod == that.limitRefreshPeriod && limitForPeriod == that.limitForPeriod
                && circuitEnable == that.circuitEnable && timeoutDuration == that.timeoutDuration && slidingWindowSize == that.slidingWindowSize
                && slidingWindowType == that.slidingWindowType && minimumNumberOfCalls == that.minimumNumberOfCalls
                && waitIntervalFunctionInOpenState == that.waitIntervalFunctionInOpenState && permittedNumberOfCallsInHalfOpenState == that.permittedNumberOfCallsInHalfOpenState
                && Float.compare(that.failureRateThreshold, failureRateThreshold) == 0 && Objects.equals(fallbackUri, that.fallbackUri)
                && Objects.equals(automaticTransitionFromOpenToHalfOpenEnabled, that.automaticTransitionFromOpenToHalfOpenEnabled);
    }

    @Override
    public int hashCode() {
        return Objects.hash(timeoutDurationRate, limitRefreshPeriod, limitForPeriod, circuitEnable, timeoutDuration, fallbackUri,
                slidingWindowSize, slidingWindowType, minimumNumberOfCalls, waitIntervalFunctionInOpenState,
                permittedNumberOfCallsInHalfOpenState, failureRateThreshold, automaticTransitionFromOpenToHalfOpenEnabled);
    }

    @Override
    public String toString() {
        return "Resilience4JHandle{"
                + "timeoutDurationRate="
                + timeoutDurationRate
                + ", limitRefreshPeriod="
                + limitRefreshPeriod
                + ", limitForPeriod="
                + limitForPeriod
                + ", circuitEnable="
                + circuitEnable
                + ", timeoutDuration="
                + timeoutDuration
                + ", fallbackUri='"
                + fallbackUri
                + '\''
                + ", slidingWindowSize="
                + slidingWindowSize
                + ", slidingWindowType="
                + slidingWindowType
                + ", minimumNumberOfCalls="
                + minimumNumberOfCalls
                + ", waitIntervalFunctionInOpenState="
                + waitIntervalFunctionInOpenState
                + ", permittedNumberOfCallsInHalfOpenState="
                + permittedNumberOfCallsInHalfOpenState
                + ", failureRateThreshold="
                + failureRateThreshold
                + ", automaticTransitionFromOpenToHalfOpenEnabled="
                + automaticTransitionFromOpenToHalfOpenEnabled
                + '}';
    }
    
    /**
     * check filed default value.
     *
     * @param resilience4JHandle {@linkplain Resilience4JHandle}
     */
    public void checkData(final Resilience4JHandle resilience4JHandle) {
        resilience4JHandle.setTimeoutDurationRate(resilience4JHandle.getTimeoutDurationRate() < 0 ? Constants.TIMEOUT_DURATION_RATE : resilience4JHandle.getTimeoutDurationRate());
        resilience4JHandle.setLimitRefreshPeriod(resilience4JHandle.getLimitRefreshPeriod() < 0 ? Constants.LIMIT_REFRESH_PERIOD : resilience4JHandle.getLimitRefreshPeriod());
        resilience4JHandle.setLimitForPeriod(resilience4JHandle.getLimitForPeriod() < 0 ? Constants.LIMIT_FOR_PERIOD : resilience4JHandle.getLimitForPeriod());
        resilience4JHandle.setCircuitEnable(resilience4JHandle.getCircuitEnable() != Constants.CIRCUIT_ENABLE ? Constants.CIRCUIT_DISABLE : Constants.CIRCUIT_ENABLE);
        resilience4JHandle.setTimeoutDuration(resilience4JHandle.getTimeoutDuration() < 0 ? Constants.TIMEOUT_DURATION : resilience4JHandle.getTimeoutDuration());
        resilience4JHandle.setFallbackUri(!"0".equals(resilience4JHandle.getFallbackUri()) ? resilience4JHandle.getFallbackUri() : "");
        resilience4JHandle.setSlidingWindowSize(resilience4JHandle.getSlidingWindowSize() < 0 ? Constants.SLIDING_WINDOW_SIZE : resilience4JHandle.getSlidingWindowSize());
        resilience4JHandle.setSlidingWindowType(resilience4JHandle.getSlidingWindowType() < 0 ? Constants.SLIDING_WINDOW_TYPE : resilience4JHandle.getSlidingWindowType());
        resilience4JHandle.setMinimumNumberOfCalls(resilience4JHandle.getMinimumNumberOfCalls() < 0 ? Constants.MINIMUM_NUMBER_OF_CALLS : resilience4JHandle.getMinimumNumberOfCalls());
        resilience4JHandle.setWaitIntervalFunctionInOpenState(resilience4JHandle.getWaitIntervalFunctionInOpenState() < 0
                ? Constants.WAIT_INTERVAL_FUNCTION_IN_OPEN_STATE : resilience4JHandle.getWaitIntervalFunctionInOpenState());
        resilience4JHandle.setPermittedNumberOfCallsInHalfOpenState(resilience4JHandle.getPermittedNumberOfCallsInHalfOpenState() < 0
                ? Constants.PERMITTED_NUMBER_OF_CALLS_IN_HALF_OPEN_STATE : resilience4JHandle.getPermittedNumberOfCallsInHalfOpenState());
        resilience4JHandle.setFailureRateThreshold(
                resilience4JHandle.getFailureRateThreshold() < 0 || resilience4JHandle.getFailureRateThreshold() > 100
                        ? Constants.FAILURE_RATE_THRESHOLD : resilience4JHandle.getFailureRateThreshold());
    }
}
