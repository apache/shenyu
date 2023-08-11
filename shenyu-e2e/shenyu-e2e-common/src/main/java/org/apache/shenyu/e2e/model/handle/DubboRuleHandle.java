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

package org.apache.shenyu.e2e.model.handle;

/**
 * Alibaba Dubbo rule handle.
 */
public class DubboRuleHandle implements RuleHandle {

    private String loadBalance;

    private int retries;

    private long timeout;

    /**
     * no args constructor.
     */
    public DubboRuleHandle() {
    }

    public DubboRuleHandle(final Builder builder) {
        this.loadBalance = builder.loadBalance;
        this.retries = builder.retries;
        this.timeout = builder.timeout;
    }
    
    /**
     * builder.
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * get loadBalance.
     *
     * @return loadBalance
     */
    public String getLoadBalance() {
        return loadBalance;
    }

    /**
     * set loadBalance.
     *
     * @param loadBalance loadBalance
     */
    public void setLoadBalance(final String loadBalance) {
        this.loadBalance = loadBalance;
    }

    /**
     * get path.
     *
     * @return path
     */
    public int getRetries() {
        return retries;
    }

    /**
     * set retries.
     *
     * @param retries retries
     */
    public void setRetries(final int retries) {
        this.retries = retries;
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
     * class builder.
     */
    public static final class Builder {

        private String loadBalance;

        private int retries;

        private long timeout;

        /**
         * no args constructor.
         */
        private Builder() {
        }
        
        /**
         * build.
         * @return SpringCloudRuleHandle
         */
        public DubboRuleHandle build() {
            return new DubboRuleHandle(this);
        }

        /**
         * build loadBalance.
         *
         * @param loadBalance loadBalance
         * @return this
         */
        public Builder loadBalance(final String loadBalance) {
            this.loadBalance = loadBalance;
            return this;
        }

        /**
         * build retries.
         *
         * @param retries retries
         * @return this
         */
        public Builder retries(final int retries) {
            this.retries = retries;
            return this;
        }

        /**
         * build timeout.
         *
         * @param timeout timeout
         * @return this
         */
        public Builder timeout(final long timeout) {
            this.timeout = timeout;
            return this;
        }
    }
}
