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

package org.apache.shenyu.common.dto.convert;

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
    private String callBackUri;

    /**
     * Isolation strategy to use when executing a hystrix command.
     */
    private int executionIsolationStrategy = HystrixIsolationModeEnum.SEMAPHORE.getCode();

    /**
     * hystrix thread pool config.
     */
    private HystrixThreadPoolConfig hystrixThreadPoolConfig;

    /**
     * get groupKey.
     *
     * @return groupKey
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
     * @return commandKey
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
     * @return maxConcurrentRequests
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
     * @return errorThresholdPercentage
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
     * @return requestVolumeThreshold
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
     * @return sleepWindowInMilliseconds
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
     * @return timeout
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
     * @return callBackUri
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
     * @return executionIsolationStrategy
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
     * @return hystrixThreadPoolConfig
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
        if (o == null || getClass() != o.getClass()) {
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
}
