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

import java.util.Objects;

/**
 * hystrix thread pool config.
 */
public class HystrixThreadPoolConfig {
    private int coreSize = Constants.HYSTRIX_THREAD_POOL_CORE_SIZE;

    private int maximumSize = Constants.HYSTRIX_THREAD_POOL_MAX_SIZE;

    private int keepAliveTimeMinutes = Constants.HYSTRIX_THREAD_KEEP_ALIVE_TIME_MINUTE;

    private int maxQueueSize = Constants.HYSTRIX_THREAD_POOL_QUEUE_SIZE;

    /**
     * get coreSize.
     *
     * @return coreSize
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
     * @return maximumSize
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
     * @return keepAliveTimeMinutes
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
     * @return maxQueueSize
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
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        HystrixThreadPoolConfig that = (HystrixThreadPoolConfig) o;
        return coreSize == that.coreSize && maximumSize == that.maximumSize
                && keepAliveTimeMinutes == that.keepAliveTimeMinutes && maxQueueSize == that.maxQueueSize;
    }

    @Override
    public int hashCode() {
        return Objects.hash(coreSize, maximumSize, keepAliveTimeMinutes, maxQueueSize);
    }

    @Override
    public String toString() {
        return "HystrixThreadPoolConfig{"
                + "coreSize="
                + coreSize
                + ", maximumSize="
                + maximumSize
                + ", keepAliveTimeMinutes="
                + keepAliveTimeMinutes
                + ", maxQueueSize="
                + maxQueueSize
                + '}';
    }
}
