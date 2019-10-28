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

package org.dromara.soul.fusing.hystrix.builder;

import com.netflix.hystrix.HystrixCommand;
import com.netflix.hystrix.HystrixCommandGroupKey;
import com.netflix.hystrix.HystrixCommandKey;
import com.netflix.hystrix.HystrixCommandProperties;
import com.netflix.hystrix.HystrixObservableCommand;
import org.dromara.soul.fusing.api.config.FusingConfig;

/**
 * the hystrix builder.
 *
 * @author xiaoyu
 */
public class HystrixBuilder {

    /**
     * this is build HystrixObservableCommand.Setter.
     *
     * @param commonHystrix {@linkplain CommonHystrix}
     * @return {@linkplain HystrixObservableCommand.Setter}
     */
    public static HystrixCommand.Setter build(final FusingConfig commonHystrix) {

        if (commonHystrix.getMaxConcurrentRequests() == 0) {
            commonHystrix.setMaxConcurrentRequests(DefaultHystrix.MAX_CONCURRENT_REQUESTS);
        }
        if (commonHystrix.getErrorThresholdPercentage() == 0) {
            commonHystrix.setErrorThresholdPercentage(DefaultHystrix.ERROR_THRESHOLD_PERCENTAGE);
        }
        if (commonHystrix.getRequestVolumeThreshold() == 0) {
            commonHystrix.setRequestVolumeThreshold(DefaultHystrix.REQUEST_VOLUME_THRESHOLD);
        }
        if (commonHystrix.getSleepWindowInMilliseconds() == 0) {
            commonHystrix.setSleepWindowInMilliseconds(DefaultHystrix.SLEEP_WINDOW_INMILLISECONDS);
        }

        HystrixCommandGroupKey groupKey = HystrixCommandGroupKey.Factory.asKey(commonHystrix.getGroupKey());

        HystrixCommandKey commandKey = HystrixCommandKey.Factory.asKey(commonHystrix.getCommandKey());

        final HystrixCommandProperties.Setter propertiesSetter =
                HystrixCommandProperties.Setter()
                        .withExecutionTimeoutInMilliseconds(commonHystrix.getTimeout())
                        .withCircuitBreakerEnabled(true)
                        .withExecutionIsolationStrategy(HystrixCommandProperties.ExecutionIsolationStrategy.SEMAPHORE)
                        .withExecutionIsolationSemaphoreMaxConcurrentRequests(commonHystrix.getMaxConcurrentRequests())
                        .withCircuitBreakerErrorThresholdPercentage(commonHystrix.getErrorThresholdPercentage())
                        .withCircuitBreakerRequestVolumeThreshold(commonHystrix.getRequestVolumeThreshold())
                        .withCircuitBreakerSleepWindowInMilliseconds(commonHystrix.getSleepWindowInMilliseconds());

        return HystrixCommand.Setter.withGroupKey(groupKey)
                .andCommandKey(commandKey)
                .andCommandPropertiesDefaults(propertiesSetter);

    }

}
