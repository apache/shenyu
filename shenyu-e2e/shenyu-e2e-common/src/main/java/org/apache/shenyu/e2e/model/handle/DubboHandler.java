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

public class DubboHandler implements PluginHandle {

    /**
     * upstreamHost.
     */
    private String upstreamHost;

    private String protocol;

    private String upstreamUrl;

    private String gray;

    private boolean status;

    /**
     * startup time.
     */
    private long timestamp;

    private long warmup;

    /**
     * weight.
     */
    private int weight;

    public DubboHandler() {
    }

    public DubboHandler(final Builder builder) {
        this.upstreamHost = builder.upstreamHost;
        this.protocol = builder.protocol;
        this.upstreamUrl = builder.upstreamUrl;
        this.gray = builder.gray;
        this.status = builder.status;
        this.timestamp = builder.timestamp;
        this.warmup = builder.warmup;
        this.weight = builder.weight;
    }

    /**
     * builder.
     *
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * get upstreamHost.
     *
     * @return upstreamHost
     */
    public String getUpstreamHost() {
        return upstreamHost;
    }

    /**
     * set upstreamHost.
     *
     * @param upstreamHost upstreamHost
     */
    public void setUpstreamHost(final String upstreamHost) {
        this.upstreamHost = upstreamHost;
    }

    /**
     * get protocol.
     * @return protocol
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * set protocol.
     *
     * @param protocol protocol
     */
    public void setProtocol(final String protocol) {
        this.protocol = protocol;
    }

    /**
     * get upstreamUrl.
     *
     * @return upstreamUrl
     */
    public String getUpstreamUrl() {
        return upstreamUrl;
    }

    /**
     * set upstreamUrl.
     *
     * @param upstreamUrl upstreamUrl
     */
    public void setUpstreamUrl(final String upstreamUrl) {
        this.upstreamUrl = upstreamUrl;
    }

    /**
     * get gray.
     *
     * @return gray
     */
    public String getGray() {
        return gray;
    }

    /**
     * set gray.
     *
     * @param gray gray
     */
    public void setGray(final String gray) {
        this.gray = gray;
    }

    /**
     * is status.
     *
     * @return status
     */
    public boolean isStatus() {
        return status;
    }

    /**
     * set status.
     * @param status status
     */
    public void setStatus(final boolean status) {
        this.status = status;
    }

    /**
     * get timestamp.
     *
     * @return timestamp
     */
    public long getTimestamp() {
        return timestamp;
    }

    /**
     * set timestamp.
     *
     * @param timestamp timestamp
     */
    public void setTimestamp(final long timestamp) {
        this.timestamp = timestamp;
    }

    /**
     * get warmup.
     *
     * @return warmup
     */
    public long getWarmup() {
        return warmup;
    }

    /**
     * set warmup.
     *
     * @param warmup warmup
     */
    public void setWarmup(final long warmup) {
        this.warmup = warmup;
    }

    /**
     * get weight.
     *
     * @return weight
     */
    public int getWeight() {
        return weight;
    }

    /**
     * set weight.
     *
     * @param weight weight
     */
    public void setWeight(final int weight) {
        this.weight = weight;
    }

    public static final class Builder {
        /**
         * upstreamHost.
         */
        private String upstreamHost;

        private String protocol;

        private String upstreamUrl;

        private String gray;

        private boolean status;

        /**
         * startup time.
         */
        private long timestamp;

        private long warmup;

        /**
         * weight.
         */
        private int weight;

        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return DubboHandler
         */
        public DubboHandler build() {
            return new DubboHandler(this);
        }

        /**
         * build upstreamHost.
         *
         * @param upstreamHost upstreamHost
         * @return upstreamHost
         */
        public Builder upstreamHost(final String upstreamHost) {
            this.upstreamHost = upstreamHost;
            return this;
        }

        /**
         * build protocol.
         *
         * @param protocol protocol
         * @return protocol
         */
        public Builder protocol(final String protocol) {
            this.protocol = protocol;
            return this;
        }

        /**
         * build upstreamUrl.
         *
         * @param upstreamUrl upstreamUrl
         * @return upstreamUrl
         */
        public Builder upstreamUrl(final String upstreamUrl) {
            this.upstreamUrl = upstreamUrl;
            return this;
        }

        /**
         * build gray.
         *
         * @param gray gray
         * @return gray
         */
        public Builder gray(final String gray) {
            this.gray = gray;
            return this;
        }

        /**
         * build status.
         *
         * @param status status
         * @return status
         */
        public Builder status(final boolean status) {
            this.status = status;
            return this;
        }

        /**
         * build timestamp.
         *
         * @param timestamp timestamp
         * @return timestamp
         */
        public Builder timestamp(final long timestamp) {
            this.timestamp = timestamp;
            return this;
        }

        /**
         * build warmup.
         *
         * @param warmup warmup
         * @return warmup
         */
        public Builder warmup(final long warmup) {
            this.warmup = warmup;
            return this;
        }

        /**
         * build weight.
         *
         * @param weight weight
         * @return weight
         */
        public Builder weight(final int weight) {
            this.weight = weight;
            return this;
        }
    }
}
