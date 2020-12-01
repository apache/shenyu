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

package org.dromara.soul.common.dto.convert;

import lombok.Data;
import lombok.EqualsAndHashCode;
import org.dromara.soul.common.constant.Constants;

/**
 * this is Resilience4J plugin handle.
 *
 * @author zhanglei
 */
@Data
@EqualsAndHashCode
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
    private int circuitEnable = Constants.CIRCUIT_ENABLE;

    /**
     * circuitBreaker timeoutDuration.
     */
    private long timeoutDuration = Constants.TIMEOUT_DURATION;

    /**
     * circuitBreaker timeoutDuration.
     */
    private String fallbackUri;

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
     * circuitBreaker waitIntervalFunctionInOpenState.
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
}
