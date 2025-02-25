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
import org.apache.shenyu.common.enums.HystrixIsolationModeEnum;

import java.util.Objects;

/**
 * this is hystrix handle.
 */
public class HystrixHandle {

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
    private int maxConcurrentRequests = Constants.MAX_CONCURRENT_REQUESTS;

    /**
     * hystrix withCircuitBreakerErrorThresholdPercentage.
     */
    private int errorThresholdPercentage = Constants.ERROR_THRESHOLD_PERCENTAGE;

    /**
     * hystrix withCircuitBreakerRequestVolumeThreshold.
     */
    private int requestVolumeThreshold = Constants.REQUEST_VOLUME_THRESHOLD;

    /**
     * hystrix withCircuitBreakerSleepWindowInMilliseconds.
     */
    private int sleepWindowInMilliseconds = Constants.SLEEP_WINDOW_INMILLISECONDS;

    /**
     * timeout is required.
     */
    private long timeout = Constants.TIME_OUT;

    /**
     * call back uri.
     * when some error occurs in hystrix invoke it will forward to this
     */
    private String callBackUri = "/fallback/hystrix";

    /**
     * Isolation strategy to use when executing a hystrix command.
     */
    private int executionIsolationStrategy = HystrixIsolationModeEnum.SEMAPHORE.getCode();

    /**
     * hystrix thread pool config.
     */
    private HystrixThreadPoolConfig hystrixThreadPoolConfig;
    
    /**
     * New instance hystrix handle.
     *
     * @return the hystrix handle
     */
    public static HystrixHandle newDefaultInstance() {
        return new HystrixHandle();
    }
    
    /**
     * get groupKey.
     *
     * @return groupKey group key
     */
    public String getGroupKey() {
        return groupKey;
    }
    
    /**
     * set groupKey.
     *
     * @param groupKey groupKey
     */
    public void setGroupKey(final String groupKey) {
        this.groupKey = groupKey;
    }
    
    /**
     * get commandKey.
     *
     * @return commandKey command key
     */
    public String getCommandKey() {
        return commandKey;
    }
    
    /**
     * set commandKey.
     *
     * @param commandKey commandKey
     */
    public void setCommandKey(final String commandKey) {
        this.commandKey = commandKey;
    }
    
    /**
     * get maxConcurrentRequests.
     *
     * @return maxConcurrentRequests max concurrent requests
     */
    public int getMaxConcurrentRequests() {
        return maxConcurrentRequests;
    }
    
    /**
     * set maxConcurrentRequests.
     *
     * @param maxConcurrentRequests maxConcurrentRequests
     */
    public void setMaxConcurrentRequests(final int maxConcurrentRequests) {
        this.maxConcurrentRequests = maxConcurrentRequests;
    }
    
    /**
     * get errorThresholdPercentage.
     *
     * @return errorThresholdPercentage error threshold percentage
     */
    public int getErrorThresholdPercentage() {
        return errorThresholdPercentage;
    }
    
    /**
     * set errorThresholdPercentage.
     *
     * @param errorThresholdPercentage errorThresholdPercentage
     */
    public void setErrorThresholdPercentage(final int errorThresholdPercentage) {
        this.errorThresholdPercentage = errorThresholdPercentage;
    }
    
    /**
     * get requestVolumeThreshold.
     *
     * @return requestVolumeThreshold request volume threshold
     */
    public int getRequestVolumeThreshold() {
        return requestVolumeThreshold;
    }
    
    /**
     * set requestVolumeThreshold.
     *
     * @param requestVolumeThreshold requestVolumeThreshold
     */
    public void setRequestVolumeThreshold(final int requestVolumeThreshold) {
        this.requestVolumeThreshold = requestVolumeThreshold;
    }
    
    /**
     * get sleepWindowInMilliseconds.
     *
     * @return sleepWindowInMilliseconds sleep window in milliseconds
     */
    public int getSleepWindowInMilliseconds() {
        return sleepWindowInMilliseconds;
    }
    
    /**
     * set sleepWindowInMilliseconds.
     *
     * @param sleepWindowInMilliseconds sleepWindowInMilliseconds
     */
    public void setSleepWindowInMilliseconds(final int sleepWindowInMilliseconds) {
        this.sleepWindowInMilliseconds = sleepWindowInMilliseconds;
    }
    
    /**
     * get timeout.
     *
     * @return timeout timeout
     */
    public long getTimeout() {
        return timeout;
    }
    
    /**
     * set timeout.
     *
     * @param timeout timeout
     */
    public void setTimeout(final long timeout) {
        this.timeout = timeout;
    }
    
    /**
     * get callBackUri.
     *
     * @return callBackUri call back uri
     */
    public String getCallBackUri() {
        return callBackUri;
    }
    
    /**
     * set callBackUri.
     *
     * @param callBackUri callBackUri
     */
    public void setCallBackUri(final String callBackUri) {
        this.callBackUri = callBackUri;
    }
    
    /**
     * get executionIsolationStrategy.
     *
     * @return executionIsolationStrategy execution isolation strategy
     */
    public int getExecutionIsolationStrategy() {
        return executionIsolationStrategy;
    }
    
    /**
     * set executionIsolationStrategy.
     *
     * @param executionIsolationStrategy executionIsolationStrategy
     */
    public void setExecutionIsolationStrategy(final int executionIsolationStrategy) {
        this.executionIsolationStrategy = executionIsolationStrategy;
    }
    
    /**
     * get hystrixThreadPoolConfig.
     *
     * @return hystrixThreadPoolConfig hystrix thread pool config
     */
    public HystrixThreadPoolConfig getHystrixThreadPoolConfig() {
        return hystrixThreadPoolConfig;
    }
    
    /**
     * set hystrixThreadPoolConfig.
     *
     * @param hystrixThreadPoolConfig hystrixThreadPoolConfig
     */
    public void setHystrixThreadPoolConfig(final HystrixThreadPoolConfig hystrixThreadPoolConfig) {
        this.hystrixThreadPoolConfig = hystrixThreadPoolConfig;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        HystrixHandle that = (HystrixHandle) o;
        return maxConcurrentRequests == that.maxConcurrentRequests && errorThresholdPercentage == that.errorThresholdPercentage
                && requestVolumeThreshold == that.requestVolumeThreshold && sleepWindowInMilliseconds == that.sleepWindowInMilliseconds
                && timeout == that.timeout && executionIsolationStrategy == that.executionIsolationStrategy && Objects.equals(groupKey, that.groupKey)
                && Objects.equals(commandKey, that.commandKey) && Objects.equals(callBackUri, that.callBackUri)
                && Objects.equals(hystrixThreadPoolConfig, that.hystrixThreadPoolConfig);
    }

    @Override
    public int hashCode() {
        return Objects.hash(groupKey, commandKey, maxConcurrentRequests, errorThresholdPercentage, requestVolumeThreshold,
                sleepWindowInMilliseconds, timeout, callBackUri, executionIsolationStrategy, hystrixThreadPoolConfig);
    }

    @Override
    public String toString() {
        return "HystrixHandle{"
                + "groupKey='"
                + groupKey
                + '\''
                + ", commandKey='"
                + commandKey
                + '\''
                + ", maxConcurrentRequests="
                + maxConcurrentRequests
                + ", errorThresholdPercentage="
                + errorThresholdPercentage
                + ", requestVolumeThreshold="
                + requestVolumeThreshold
                + ", sleepWindowInMilliseconds="
                + sleepWindowInMilliseconds
                + ", timeout="
                + timeout
                + ", callBackUri='"
                + callBackUri
                + '\''
                + ", executionIsolationStrategy="
                + executionIsolationStrategy
                + ", hystrixThreadPoolConfig="
                + hystrixThreadPoolConfig
                + '}';
    }
    
    /**
     * hystrix thread pool config.
     */
    public static class HystrixThreadPoolConfig {
    
        private int coreSize = Constants.HYSTRIX_THREAD_POOL_CORE_SIZE;
    
        private int maximumSize = Constants.HYSTRIX_THREAD_POOL_MAX_SIZE;
    
        private int keepAliveTimeMinutes = Constants.HYSTRIX_THREAD_KEEP_ALIVE_TIME_MINUTE;
    
        private int maxQueueSize = Constants.HYSTRIX_THREAD_POOL_QUEUE_SIZE;
    
        /**
         * get coreSize.
         *
         * @return coreSize core size
         */
        public int getCoreSize() {
            return coreSize;
        }
    
        /**
         * set coreSize.
         *
         * @param coreSize coreSize
         */
        public void setCoreSize(final int coreSize) {
            this.coreSize = coreSize;
        }
    
        /**
         * get maximumSize.
         *
         * @return maximumSize maximum size
         */
        public int getMaximumSize() {
            return maximumSize;
        }
    
        /**
         * set maximumSize.
         *
         * @param maximumSize maximumSize
         */
        public void setMaximumSize(final int maximumSize) {
            this.maximumSize = maximumSize;
        }
    
        /**
         * get keepAliveTimeMinutes.
         *
         * @return keepAliveTimeMinutes keep alive time minutes
         */
        public int getKeepAliveTimeMinutes() {
            return keepAliveTimeMinutes;
        }
    
        /**
         * set keepAliveTimeMinutes.
         *
         * @param keepAliveTimeMinutes keepAliveTimeMinutes
         */
        public void setKeepAliveTimeMinutes(final int keepAliveTimeMinutes) {
            this.keepAliveTimeMinutes = keepAliveTimeMinutes;
        }
    
        /**
         * get maxQueueSize.
         *
         * @return maxQueueSize max queue size
         */
        public int getMaxQueueSize() {
            return maxQueueSize;
        }
    
        /**
         * set maxQueueSize.
         *
         * @param maxQueueSize maxQueueSize
         */
        public void setMaxQueueSize(final int maxQueueSize) {
            this.maxQueueSize = maxQueueSize;
        }
    
        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (Objects.isNull(o) || getClass() != o.getClass()) {
                return false;
            }
            HystrixThreadPoolConfig that = (HystrixThreadPoolConfig) o;
            return coreSize == that.coreSize && maximumSize == that.maximumSize && keepAliveTimeMinutes == that.keepAliveTimeMinutes && maxQueueSize == that.maxQueueSize;
        }
    
        @Override
        public int hashCode() {
            return Objects.hash(coreSize, maximumSize, keepAliveTimeMinutes, maxQueueSize);
        }
    
        @Override
        public String toString() {
            return "HystrixThreadPoolConfig{" + "coreSize=" + coreSize + ", maximumSize=" + maximumSize + ", keepAliveTimeMinutes=" + keepAliveTimeMinutes + ", maxQueueSize=" + maxQueueSize + '}';
        }
    }
}
