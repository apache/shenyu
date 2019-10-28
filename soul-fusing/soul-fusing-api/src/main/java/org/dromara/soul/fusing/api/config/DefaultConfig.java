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

/**
 * DefaultConfig.
 *
 * @author xiaoyu(Myth)
 */
public interface DefaultConfig {

    /**
     * hystrix withExecutionIsolationSemaphoreMaxConcurrentRequests.
     */
    int MAX_CONCURRENT_REQUESTS = 100;

    /**
     * hystrix  withCircuitBreakerErrorThresholdPercentage.
     */
    int ERROR_THRESHOLD_PERCENTAGE = 50;

    /**
     * hystrix withCircuitBreakerRequestVolumeThreshold.
     */
    int REQUEST_VOLUME_THRESHOLD = 20;

    /**
     * hystrix withCircuitBreakerSleepWindowInMilliseconds.
     */
    int SLEEP_WINDOW_INMILLISECONDS = 5000;

    /**
     * The constant TIME_OUT.
     */
    int TIME_OUT = 3000;


}

