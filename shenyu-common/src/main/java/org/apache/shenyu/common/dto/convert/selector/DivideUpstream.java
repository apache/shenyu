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

package org.apache.shenyu.common.dto.convert.selector;

import java.util.Objects;

/**
 * this is divide upstream.
 */
public class DivideUpstream {
    
    /**
     * this is http protocol.
     */
    private String protocol;

    /**
     * host.
     */
    private String upstreamHost;

    /**
     * url.
     */
    private String upstreamUrl;

    /**
     * weight.
     */
    private int weight;

    /**
     * false close/ true open.
     */
    private boolean status = true;

    /**
     * startup time.
     */
    private long timestamp;

    /**
     * warmup.
     */
    private int warmup;

    /**
     * health status.
     */
    private boolean healthy;

    /**
     * the last healthy time.
     */
    private long lastHealthTimestamp;

    /**
     * the last unhealthy time.
     */
    private long lastUnhealthyTimestamp;

    /**
     * no args constructor.
     */
    public DivideUpstream() {
    }

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private DivideUpstream(final Builder builder) {
        boolean statusValue = builder.statusValue;
        if (!builder.statusSet) {
            statusValue = DivideUpstream.defaultStatus();
        }
        this.upstreamHost = builder.upstreamHost;
        this.protocol = builder.protocol;
        this.upstreamUrl = builder.upstreamUrl;
        this.weight = builder.weight;
        this.status = statusValue;
        this.timestamp = builder.timestamp;
        this.warmup = builder.warmup;
        this.healthy = builder.healthy;
        this.lastHealthTimestamp = builder.lastHealthTimestamp;
        this.lastUnhealthyTimestamp = builder.lastUnhealthyTimestamp;
    }

    /**
     * class builder.
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
     *
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

    /**
     * get status.
     *
     * @return status
     */
    public boolean isStatus() {
        return status;
    }

    /**
     * set status.
     *
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
    public int getWarmup() {
        return warmup;
    }

    /**
     * set warmup.
     *
     * @param warmup warmup
     */
    public void setWarmup(final int warmup) {
        this.warmup = warmup;
    }

    /**
     * get healthy.
     *
     * @return healthy
     */
    public boolean isHealthy() {
        return healthy;
    }

    /**
     * set healthy.
     *
     * @param healthy healthy
     */
    public void setHealthy(final boolean healthy) {
        this.healthy = healthy;
    }

    /**
     * get lastHealthTimestamp.
     *
     * @return lastHealthTimestamp
     */
    public long getLastHealthTimestamp() {
        return lastHealthTimestamp;
    }

    /**
     * set lastHealthTimestamp.
     *
     * @param lastHealthTimestamp lastHealthTimestamp
     */
    public void setLastHealthTimestamp(final long lastHealthTimestamp) {
        this.lastHealthTimestamp = lastHealthTimestamp;
    }

    /**
     * get lastUnhealthyTimestamp.
     *
     * @return lastUnhealthyTimestamp
     */
    public long getLastUnhealthyTimestamp() {
        return lastUnhealthyTimestamp;
    }

    /**
     * set lastUnhealthyTimestamp.
     *
     * @param lastUnhealthyTimestamp lastUnhealthyTimestamp
     */
    public void setLastUnhealthyTimestamp(final long lastUnhealthyTimestamp) {
        this.lastUnhealthyTimestamp = lastUnhealthyTimestamp;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        DivideUpstream that = (DivideUpstream) o;
        return Objects.equals(upstreamHost, that.upstreamHost) && Objects.equals(protocol, that.protocol) && Objects.equals(upstreamUrl, that.upstreamUrl);
    }

    @Override
    public int hashCode() {
        return Objects.hash(upstreamHost, protocol, upstreamUrl);
    }

    @Override
    public String toString() {
        return "DivideUpstream{"
                + "upstreamHost='"
                + upstreamHost
                + '\''
                + ", protocol='"
                + protocol
                + '\''
                + ", upstreamUrl='"
                + upstreamUrl
                + '\''
                + ", weight="
                + weight
                + ", status="
                + status
                + ", timestamp="
                + timestamp
                + ", warmup="
                + warmup
                + ", healthy="
                + healthy
                + ", lastHealthTimestamp="
                + lastHealthTimestamp
                + ", lastUnhealthyTimestamp="
                + lastUnhealthyTimestamp
                + '}';
    }

    /**
     * default value for builder.
     *
     * @return status default value
     */
    private static boolean defaultStatus() {
        return true;
    }

    /**
     * class builder.
     */
    public static final class Builder {

        /**
         * upstreamHost.
         */
        private String upstreamHost;

        /**
         * protocol.
         */
        private String protocol;

        /**
         * upstreamUrl.
         */
        private String upstreamUrl;

        /**
         * weight.
         */
        private int weight;

        /**
         * status.
         */
        private boolean statusSet;

        /**
         * status.
         */
        private boolean statusValue;

        /**
         * timestamp.
         */
        private long timestamp;

        /**
         * warmup.
         */
        private int warmup;

        /**
         * healthy.
         */
        private boolean healthy;

        /**
         * lastHealthTimestamp.
         */
        private long lastHealthTimestamp;

        /**
         * lastUnhealthyTimestamp.
         */
        private long lastUnhealthyTimestamp;

        /**
         * gray.
         */
        private boolean gray;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return DivideUpstream
         */
        public DivideUpstream build() {
            return new DivideUpstream(this);
        }

        /**
         * build upstreamHost.
         *
         * @param upstreamHost upstreamHost
         * @return this
         */
        public Builder upstreamHost(final String upstreamHost) {
            this.upstreamHost = upstreamHost;
            return this;
        }

        /**
         * build protocol.
         *
         * @param protocol protocol
         * @return this
         */
        public Builder protocol(final String protocol) {
            this.protocol = protocol;
            return this;
        }

        /**
         * build upstreamUrl.
         *
         * @param upstreamUrl upstreamUrl
         * @return this
         */
        public Builder upstreamUrl(final String upstreamUrl) {
            this.upstreamUrl = upstreamUrl;
            return this;
        }

        /**
         * build weight.
         *
         * @param weight weight
         * @return this
         */
        public Builder weight(final int weight) {
            this.weight = weight;
            return this;
        }

        /**
         * build status.
         *
         * @param status status
         * @return this
         */
        public Builder status(final boolean status) {
            this.statusValue = status;
            this.statusSet = true;
            return this;
        }

        /**
         * build timestamp.
         *
         * @param timestamp timestamp
         * @return this
         */
        public Builder timestamp(final long timestamp) {
            this.timestamp = timestamp;
            return this;
        }

        /**
         * build warmup.
         *
         * @param warmup warmup
         * @return this
         */
        public Builder warmup(final int warmup) {
            this.warmup = warmup;
            return this;
        }

        /**
         * build healthy.
         *
         * @param healthy healthy
         * @return this
         */
        public Builder healthy(final boolean healthy) {
            this.healthy = healthy;
            return this;
        }

        /**
         * build lastHealthTimestamp.
         *
         * @param lastHealthTimestamp lastHealthTimestamp
         * @return this
         */
        public Builder lastHealthTimestamp(final long lastHealthTimestamp) {
            this.lastHealthTimestamp = lastHealthTimestamp;
            return this;
        }

        /**
         * build lastUnhealthyTimestamp.
         *
         * @param lastUnhealthyTimestamp lastUnhealthyTimestamp
         * @return this
         */
        public Builder lastUnhealthyTimestamp(final long lastUnhealthyTimestamp) {
            this.lastUnhealthyTimestamp = lastUnhealthyTimestamp;
            return this;
        }
    }
}
