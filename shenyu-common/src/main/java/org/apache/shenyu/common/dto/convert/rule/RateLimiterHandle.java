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

import java.util.Objects;

/**
 * this is rateLimiter plugin handle.
 */
public class RateLimiterHandle {

    /**
     * algorithm name.
     */
    private String algorithmName;

    /**
     * replenish rate.
     */
    private double replenishRate;

    /**
     * burst capacity.
     */
    private double burstCapacity;

    /**
     * request count.
     */
    private double requestCount = 1.0;

    /**
     * loged.
     */
    private boolean loged;

    /**
     * key resolver name.
     */
    private String keyResolverName;
    
    /**
     * New default instance rate limiter handle.
     *
     * @return the rate limiter handle
     */
    public static RateLimiterHandle newDefaultInstance() {
        RateLimiterHandle rateLimiterHandle = new RateLimiterHandle();
        rateLimiterHandle.setAlgorithmName("tokenBucket");
        rateLimiterHandle.setReplenishRate(1.0);
        rateLimiterHandle.setBurstCapacity(100.0);
        rateLimiterHandle.setRequestCount(1.0);
        rateLimiterHandle.setLoged(false);
        return rateLimiterHandle;
    }
    
    /**
     * get algorithmName.
     *
     * @return algorithmName algorithm name
     */
    public String getAlgorithmName() {
        return algorithmName;
    }
    
    /**
     * set algorithmName.
     *
     * @param algorithmName algorithmName
     */
    public void setAlgorithmName(final String algorithmName) {
        this.algorithmName = algorithmName;
    }
    
    /**
     * get replenishRate.
     *
     * @return replenishRate replenish rate
     */
    public double getReplenishRate() {
        return replenishRate;
    }
    
    /**
     * set replenishRate.
     *
     * @param replenishRate replenishRate
     */
    public void setReplenishRate(final double replenishRate) {
        this.replenishRate = replenishRate;
    }
    
    /**
     * get burstCapacity.
     *
     * @return burstCapacity burst capacity
     */
    public double getBurstCapacity() {
        return burstCapacity;
    }
    
    /**
     * set burstCapacity.
     *
     * @param burstCapacity burstCapacity
     */
    public void setBurstCapacity(final double burstCapacity) {
        this.burstCapacity = burstCapacity;
    }
    
    /**
     * get requestCount.
     *
     * @return requestCount request count
     */
    public double getRequestCount() {
        return requestCount;
    }
    
    /**
     * set requestCount.
     *
     * @param requestCount requestCount
     */
    public void setRequestCount(final double requestCount) {
        this.requestCount = requestCount;
    }
    
    /**
     * get loged.
     *
     * @return loged boolean
     */
    public boolean isLoged() {
        return loged;
    }
    
    /**
     * set loged.
     *
     * @param loged loged
     */
    public void setLoged(final boolean loged) {
        this.loged = loged;
    }
    
    /**
     * get keyResolverName.
     *
     * @return keyResolverName key resolver name
     */
    public String getKeyResolverName() {
        return keyResolverName;
    }
    
    /**
     * set keyResolverName.
     *
     * @param keyResolverName keyResolverName
     */
    public void setKeyResolverName(final String keyResolverName) {
        this.keyResolverName = keyResolverName;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        RateLimiterHandle that = (RateLimiterHandle) o;
        return Double.compare(that.replenishRate, replenishRate) == 0 && Double.compare(that.burstCapacity, burstCapacity) == 0
                && Double.compare(that.requestCount, requestCount) == 0 && loged == that.loged
                && Objects.equals(algorithmName, that.algorithmName) && Objects.equals(keyResolverName, that.keyResolverName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(algorithmName, replenishRate, burstCapacity, requestCount, loged, keyResolverName);
    }

    @Override
    public String toString() {
        return "RateLimiterHandle{"
                + "algorithmName='"
                + algorithmName
                + '\''
                + ", replenishRate="
                + replenishRate
                + ", burstCapacity="
                + burstCapacity
                + ", requestCount="
                + requestCount
                + ", loged="
                + loged
                + ", keyResolverName='"
                + keyResolverName
                + '\''
                + '}';
    }
}
