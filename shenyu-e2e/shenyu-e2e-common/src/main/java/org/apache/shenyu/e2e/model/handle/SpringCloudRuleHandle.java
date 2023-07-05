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
 * SpringCLoud rule handle.
 */
public class SpringCloudRuleHandle implements RuleHandle {

    private String loadBalance;

    private String path;

    private long timeout;

    /**
     * no args constructor.
     */
    public SpringCloudRuleHandle() {
    }

    public SpringCloudRuleHandle(final Builder builder) {
        this.loadBalance = builder.loadBalance;
        this.path = builder.path;
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
    public String getPath() {
        return path;
    }

    /**
     * set path.
     *
     * @param path path
     */
    public void setPath(final String path) {
        this.path = path;
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

        private String path;

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
        public SpringCloudRuleHandle build() {
            return new SpringCloudRuleHandle(this);
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
         * build path.
         *
         * @param path path
         * @return this
         */
        public Builder path(final String path) {
            this.path = path;
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
