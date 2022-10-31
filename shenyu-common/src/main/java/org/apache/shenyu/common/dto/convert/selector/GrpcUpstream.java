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
 * this is grpc upstream.
 */
public final class GrpcUpstream extends CommonUpstream {

    /**
     * weight.
     */
    private int weight;
    
    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private GrpcUpstream(final Builder builder) {
        boolean statusValue = builder.statusValue;
        if (!builder.statusSet) {
            statusValue = defaultStatus();
        }
        setUpstreamHost(builder.upstreamHost);
        setProtocol(builder.protocol);
        setUpstreamUrl(builder.upstreamUrl); 
        this.weight = builder.weight;
        setStatus(statusValue);
        setTimestamp(builder.timestamp);
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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        GrpcUpstream that = (GrpcUpstream) o;
        return Objects.equals(getUpstreamHost(), that.getUpstreamHost()) && Objects.equals(getProtocol(), that.getProtocol()) && Objects.equals(getUpstreamUrl(), that.getUpstreamUrl());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getUpstreamHost(), getProtocol(), getUpstreamUrl());
    }

    @Override
    public String toString() {
        return "GrpcUpstream{"
                + "upstreamHost='"
                + getUpstreamHost()
                + '\''
                + ", protocol='"
                + getProtocol()
                + '\''
                + ", upstreamUrl='"
                + getUpstreamUrl()
                + '\''
                + ", weight="
                + weight
                + ", status="
                + isStatus()
                + ", timestamp="
                + getTimestamp()
                + '}';
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
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return DivideUpstream
         */
        public GrpcUpstream build() {
            return new GrpcUpstream(this);
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
    }
}
