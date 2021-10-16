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

package org.apache.shenyu.loadbalancer.entity;

import java.util.Objects;

/**
 * this is upstream.
 */
public final class Upstream {

    /**
     * protocol.
     */
    private String protocol;

    /**
     * url.
     */
    private String url;

    /**
     * weight.
     */
    private int weight;

    /**
     * false close/ true open.
     */
    private boolean status;

    /**
     * startup time.
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
     * group.
     */
    private String group;

    /**
     * version.
     */
    private String version;

    private Upstream(final Builder builder) {
        this.protocol = builder.protocol;
        this.url = builder.url;
        this.weight = builder.weight;
        this.status = builder.status;
        this.timestamp = builder.timestamp;
        this.warmup = builder.warmup;
        this.group = builder.group;
        this.version = builder.version;
    }

    /**
     * Gets protocol.
     *
     * @return the protocol
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * Is status boolean.
     *
     * @return the boolean
     */
    public boolean isStatus() {
        return status;
    }

    /**
     * Sets status.
     *
     * @param status the status
     */
    public void setStatus(final boolean status) {
        this.status = status;
    }

    /**
     * Gets timestamp.
     *
     * @return the timestamp
     */
    public long getTimestamp() {
        return timestamp;
    }

    /**
     * Gets warmup.
     *
     * @return the warmup
     */
    public int getWarmup() {
        return warmup;
    }

    /**
     * Gets url.
     *
     * @return the url
     */
    public String getUrl() {
        return url;
    }

    /**
     * Sets url.
     *
     * @param url the url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * get weight.
     *
     * @return weight weight
     */
    public int getWeight() {
        return weight;
    }

    /**
     * Is healthy boolean.
     *
     * @return the boolean
     */
    public boolean isHealthy() {
        return healthy;
    }

    /**
     * Sets healthy.
     *
     * @param healthy the healthy
     */
    public void setHealthy(final boolean healthy) {
        this.healthy = healthy;
    }

    /**
     * Gets last health timestamp.
     *
     * @return the last health timestamp
     */
    public long getLastHealthTimestamp() {
        return lastHealthTimestamp;
    }

    /**
     * Sets last health timestamp.
     *
     * @param lastHealthTimestamp the last health timestamp
     */
    public void setLastHealthTimestamp(final long lastHealthTimestamp) {
        this.lastHealthTimestamp = lastHealthTimestamp;
    }

    /**
     * Gets last unhealthy timestamp.
     *
     * @return the last unhealthy timestamp
     */
    public long getLastUnhealthyTimestamp() {
        return lastUnhealthyTimestamp;
    }

    /**
     * Sets last unhealthy timestamp.
     *
     * @param lastUnhealthyTimestamp the last unhealthy timestamp
     */
    public void setLastUnhealthyTimestamp(final long lastUnhealthyTimestamp) {
        this.lastUnhealthyTimestamp = lastUnhealthyTimestamp;
    }

    /**
     * Gets group.
     *
     * @return the group
     */
    public String getGroup() {
        return group;
    }

    /**
     * Sets group.
     *
     * @param group the group
     */
    public void setGroup(final String group) {
        this.group = group;
    }

    /**
     * Gets version.
     *
     * @return the version
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets version.
     *
     * @param version the version
     */
    public void setVersion(final String version) {
        this.version = version;
    }

    /**
     * class builder.
     *
     * @return Builder builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Upstream that = (Upstream) o;
        return Objects.equals(url, that.url) && Objects.equals(protocol, that.protocol) && Objects.equals(weight, that.weight);
    }

    @Override
    public int hashCode() {
        return Objects.hash(protocol, url, weight);
    }

    @Override
    public String toString() {
        return "Upstream{"
                + "protocol='" + protocol
                + ", url='" + url
                + ", weight=" + weight
                + ", status=" + status
                + ", timestamp=" + timestamp
                + ", warmup=" + warmup
                + ", group='" + group
                + ", version='" + version
                + '}';
    }

    /**
     * class builder.
     */
    public static final class Builder {

        /**
         * protocol.
         */
        private String protocol;

        /**
         * url.
         */
        private String url;

        /**
         * weight.
         */
        private int weight = 50;

        /**
         * status.
         */
        private boolean status = true;

        /**
         * timestamp.
         */
        private long timestamp;

        /**
         * warmup.
         */
        private int warmup = 10 * 60 * 1000;

        /**
         * group.
         */
        private String group;

        /**
         * version.
         */
        private String version;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return Upstream upstream
         */
        public Upstream build() {
            return new Upstream(this);
        }

        /**
         * build protocol.
         *
         * @param protocol protocol
         * @return this builder
         */
        public Builder protocol(final String protocol) {
            this.protocol = protocol;
            return this;
        }

        /**
         * build url.
         *
         * @param url url
         * @return this builder
         */
        public Builder url(final String url) {
            this.url = url;
            return this;
        }

        /**
         * build weight.
         *
         * @param weight weight
         * @return this builder
         */
        public Builder weight(final int weight) {
            this.weight = weight;
            return this;
        }

        /**
         * build status.
         *
         * @param status status
         * @return this builder
         */
        public Builder status(final boolean status) {
            this.status = status;
            return this;
        }

        /**
         * build timestamp.
         *
         * @param timestamp timestamp
         * @return this builder
         */
        public Builder timestamp(final long timestamp) {
            this.timestamp = timestamp;
            return this;
        }

        /**
         * build warmup.
         *
         * @param warmup warmup
         * @return this builder
         */
        public Builder warmup(final int warmup) {
            this.warmup = warmup;
            return this;
        }

        /**
         * build group.
         *
         * @param group group
         * @return this builder
         */
        public Builder group(final String group) {
            this.group = group;
            return this;
        }

        /**
         * build version.
         *
         * @param version version
         * @return this builder
         */
        public Builder version(final String version) {
            this.version = version;
            return this;
        }
    }
}
