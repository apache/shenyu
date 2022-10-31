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

package org.apache.shenyu.plugin.hystrix.builder;

import com.netflix.hystrix.HystrixCommand;
import com.netflix.hystrix.HystrixCommandGroupKey;
import com.netflix.hystrix.HystrixCommandKey;
import com.netflix.hystrix.HystrixCommandProperties;
import com.netflix.hystrix.HystrixObservableCommand;
import com.netflix.hystrix.HystrixThreadPoolProperties;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.HystrixHandle;
import org.apache.shenyu.common.dto.convert.rule.HystrixHandle.HystrixThreadPoolConfig;

import java.util.Objects;

/**
 * the hystrix builder.
 */
public final class HystrixBuilder {
    
    private HystrixBuilder() {
    }

    /**
     * this is build HystrixObservableCommand.Setter.
     *
     * @param hystrixHandle {@linkplain HystrixHandle}
     * @param commandKey commandKey
     * @param groupKey groupKey
     * @return {@linkplain HystrixObservableCommand.Setter}
     */
    public static HystrixObservableCommand.Setter build(final HystrixHandle hystrixHandle, final String commandKey, final String groupKey) {
        initHystrixHandleOnRequire(hystrixHandle);
        HystrixCommandGroupKey hystrixCommandGroupKey = HystrixCommandGroupKey.Factory.asKey(groupKey);
        HystrixCommandKey hystrixCommandKey = HystrixCommandKey.Factory.asKey(commandKey);
        HystrixCommandProperties.Setter propertiesSetter =
                HystrixCommandProperties.Setter()
                        .withExecutionTimeoutInMilliseconds((int) hystrixHandle.getTimeout())
                        .withCircuitBreakerEnabled(true)
                        .withExecutionIsolationStrategy(HystrixCommandProperties.ExecutionIsolationStrategy.SEMAPHORE)
                        .withExecutionIsolationSemaphoreMaxConcurrentRequests(hystrixHandle.getMaxConcurrentRequests())
                        .withCircuitBreakerErrorThresholdPercentage(hystrixHandle.getErrorThresholdPercentage())
                        .withCircuitBreakerRequestVolumeThreshold(hystrixHandle.getRequestVolumeThreshold())
                        .withCircuitBreakerSleepWindowInMilliseconds(hystrixHandle.getSleepWindowInMilliseconds());
        return HystrixObservableCommand.Setter
                .withGroupKey(hystrixCommandGroupKey)
                .andCommandKey(hystrixCommandKey)
                .andCommandPropertiesDefaults(propertiesSetter);
    }

    /**
     * this is build HystrixObservableCommand.Setter.
     *
     * @param hystrixHandle {@linkplain HystrixHandle}
     * @return {@linkplain HystrixObservableCommand.Setter}
     */
    public static HystrixObservableCommand.Setter build(final HystrixHandle hystrixHandle) {
        return build(hystrixHandle, hystrixHandle.getCommandKey(), hystrixHandle.getGroupKey());
    }

    /**
     * this is build HystrixCommand.Setter.
     *
     * @param hystrixHandle {@linkplain HystrixHandle}
     * @return {@linkplain HystrixCommand.Setter}
     */
    public static HystrixCommand.Setter buildForHystrixCommand(final HystrixHandle hystrixHandle) {
        return buildForHystrixCommand(hystrixHandle, hystrixHandle.getCommandKey(), hystrixHandle.getGroupKey());
    }

    /**
     * this is build HystrixCommand.Setter.
     *
     * @param hystrixHandle {@linkplain HystrixHandle}
     * @param commandKey commandKey
     * @param groupKey groupKey
     * @return {@linkplain HystrixCommand.Setter}
     */
    public static HystrixCommand.Setter buildForHystrixCommand(final HystrixHandle hystrixHandle, final String commandKey, final String groupKey) {
        initHystrixHandleOnRequire(hystrixHandle);
        HystrixCommandGroupKey hystrixCommandGroupKey = HystrixCommandGroupKey.Factory.asKey(groupKey);
        HystrixCommandKey hystrixCommandKey = HystrixCommandKey.Factory.asKey(commandKey);
        HystrixCommandProperties.Setter propertiesSetter =
                HystrixCommandProperties.Setter()
                        .withExecutionTimeoutInMilliseconds((int) hystrixHandle.getTimeout())
                        .withCircuitBreakerEnabled(true)
                        .withCircuitBreakerErrorThresholdPercentage(hystrixHandle.getErrorThresholdPercentage())
                        .withCircuitBreakerRequestVolumeThreshold(hystrixHandle.getRequestVolumeThreshold())
                        .withCircuitBreakerSleepWindowInMilliseconds(hystrixHandle.getSleepWindowInMilliseconds());
        HystrixThreadPoolConfig hystrixThreadPoolConfig = hystrixHandle.getHystrixThreadPoolConfig();
        HystrixThreadPoolProperties.Setter threadPoolPropertiesSetter =
                HystrixThreadPoolProperties.Setter()
                        .withCoreSize(hystrixThreadPoolConfig.getCoreSize())
                        .withMaximumSize(hystrixThreadPoolConfig.getMaximumSize())
                        .withMaxQueueSize(hystrixThreadPoolConfig.getMaxQueueSize())
                        .withKeepAliveTimeMinutes(hystrixThreadPoolConfig.getKeepAliveTimeMinutes())
                        .withAllowMaximumSizeToDivergeFromCoreSize(true);
        return HystrixCommand.Setter
                .withGroupKey(hystrixCommandGroupKey)
                .andCommandKey(hystrixCommandKey)
                .andCommandPropertiesDefaults(propertiesSetter)
                .andThreadPoolPropertiesDefaults(threadPoolPropertiesSetter);
    }

    private static void initHystrixHandleOnRequire(final HystrixHandle hystrixHandle) {
        if (hystrixHandle.getMaxConcurrentRequests() == 0) {
            hystrixHandle.setMaxConcurrentRequests(Constants.MAX_CONCURRENT_REQUESTS);
        }
        if (hystrixHandle.getErrorThresholdPercentage() == 0) {
            hystrixHandle.setErrorThresholdPercentage(Constants.ERROR_THRESHOLD_PERCENTAGE);
        }
        if (hystrixHandle.getRequestVolumeThreshold() == 0) {
            hystrixHandle.setRequestVolumeThreshold(Constants.REQUEST_VOLUME_THRESHOLD);
        }
        if (hystrixHandle.getSleepWindowInMilliseconds() == 0) {
            hystrixHandle.setSleepWindowInMilliseconds(Constants.SLEEP_WINDOW_INMILLISECONDS);
        }
        if (Objects.isNull(hystrixHandle.getHystrixThreadPoolConfig())) {
            hystrixHandle.setHystrixThreadPoolConfig(new HystrixThreadPoolConfig());
        }
        HystrixThreadPoolConfig hystrixThreadPoolConfig = hystrixHandle.getHystrixThreadPoolConfig();
        if (hystrixThreadPoolConfig.getCoreSize() == 0) {
            hystrixThreadPoolConfig.setCoreSize(Constants.HYSTRIX_THREAD_POOL_CORE_SIZE);
        }
        if (hystrixThreadPoolConfig.getMaximumSize() == 0) {
            hystrixThreadPoolConfig.setMaximumSize(Constants.HYSTRIX_THREAD_POOL_MAX_SIZE);
        }
        if (hystrixThreadPoolConfig.getMaxQueueSize() == 0) {
            hystrixThreadPoolConfig.setMaxQueueSize(Constants.HYSTRIX_THREAD_POOL_QUEUE_SIZE);
        }
    }
}
