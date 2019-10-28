/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.fusing.api.config;

import lombok.Data;

import java.io.Serializable;

/**
 * @author xiaoyu
 */
@Data
public class FusingConfig implements Serializable {

    /**
     * hystrix group key is required.
     */
    private String groupKey;

    /**
     * hystrix command key is required.
     */
    private String commandKey;

    /**
     * hystrix withExecutionIsolationSemaphoreMaxConcurrentRequests.
     */
    private int maxConcurrentRequests = DefaultConfig.MAX_CONCURRENT_REQUESTS;

    /**
     * hystrix withCircuitBreakerErrorThresholdPercentage.
     */
    private int errorThresholdPercentage = DefaultConfig.ERROR_THRESHOLD_PERCENTAGE;

    /**
     * hystrix withCircuitBreakerRequestVolumeThreshold.
     */
    private int requestVolumeThreshold = DefaultConfig.REQUEST_VOLUME_THRESHOLD;

    /**
     * hystrix withCircuitBreakerSleepWindowInMilliseconds.
     */
    private int sleepWindowInMilliseconds = DefaultConfig.SLEEP_WINDOW_INMILLISECONDS;

    /**
     * timeout is required.
     */
    private Integer timeout = DefaultConfig.TIME_OUT;
}
