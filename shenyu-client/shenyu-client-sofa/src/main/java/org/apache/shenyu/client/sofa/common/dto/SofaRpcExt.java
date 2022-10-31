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

package org.apache.shenyu.client.sofa.common.dto;

/**
 * The type Sofa rpc ext.
 */
public class SofaRpcExt {

    private String loadbalance;

    private Integer retries;

    private Integer timeout;

    /**
     * constructor without params.
     */
    public SofaRpcExt() {
    }

    /**
     * constructor with all params.
     *
     * @param loadbalance loadbalance
     * @param retries retries
     * @param timeout timeout
     */
    public SofaRpcExt(final String loadbalance, final Integer retries, final Integer timeout) {
        this.loadbalance = loadbalance;
        this.retries = retries;
        this.timeout = timeout;
    }

    /**
     * get loadbalance.
     *
     * @return loadbalance
     */
    public String getLoadbalance() {
        return loadbalance;
    }

    /**
     * set loadbalance.
     *
     * @param loadbalance loadbalance
     */
    public void setLoadbalance(final String loadbalance) {
        this.loadbalance = loadbalance;
    }

    /**
     * get retries.
     *
     * @return retries
     */
    public Integer getRetries() {
        return retries;
    }

    /**
     * set retries.
     *
     * @param retries retries
     */
    public void setRetries(final Integer retries) {
        this.retries = retries;
    }

    /**
     * get timeout.
     *
     * @return timeout
     */
    public Integer getTimeout() {
        return timeout;
    }

    /**
     * set timeout.
     *
     * @param timeout timeout
     */
    public void setTimeout(final Integer timeout) {
        this.timeout = timeout;
    }

    @Override
    public String toString() {
        return "SofaRpcExt{"
                + "loadbalance='" + loadbalance + '\''
                + ", retries=" + retries
                + ", timeout=" + timeout
                + '}';
    }

    /**
     * get the Builder of SofaRpcExt.
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * the Builder of SofaRpcExt.
     */
    public static final class Builder {

        private String loadbalance;

        private Integer retries;

        private Integer timeout;

        /**
         * constructor without params.
         */
        private Builder() {
        }

        /**
         * set loadbalance.
         *
         * @param loadbalance loadbalance
         * @return Builder
         */
        public Builder loadbalance(final String loadbalance) {
            this.loadbalance = loadbalance;
            return this;
        }

        /**
         * set retries.
         *
         * @param retries retries
         * @return Builder
         */
        public Builder retries(final Integer retries) {
            this.retries = retries;
            return this;
        }

        /**
         * set timeout.
         *
         * @param timeout timeout
         * @return Builder
         */
        public Builder timeout(final Integer timeout) {
            this.timeout = timeout;
            return this;
        }

        /**
         * build SofaRpcExt.
         *
         * @return SofaRpcExt
         */
        public SofaRpcExt build() {
            SofaRpcExt sofaRpcExt = new SofaRpcExt();
            sofaRpcExt.setLoadbalance(loadbalance);
            sofaRpcExt.setRetries(retries);
            sofaRpcExt.setTimeout(timeout);
            return sofaRpcExt;
        }
    }
}
