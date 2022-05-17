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
 * this is WebSocket upstream.
 */
public final class WebSocketUpstream extends CommonUpstream {

    /**
     * host.
     */
    private String host;

    /**
     * url.
     */
    private String url;

    /**
     * weight.
     */
    private int weight;

    /**
     * warmup.
     */
    private int warmup;

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private WebSocketUpstream(final Builder builder) {
        this.host = builder.host;
        setUpstreamHost(builder.host);
        setProtocol(builder.protocol);
        this.url = builder.url;
        setUpstreamUrl(builder.url);
        this.weight = builder.weight;
        setStatus(builder.status);
        setTimestamp(builder.timestamp);
        this.warmup = builder.warmup;
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
     * get host.
     *
     * @return host
     */
    public String getHost() {
        return host;
    }

    /**
     * set host.
     *
     * @param host upstreamHost
     */
    public void setHost(final String host) {
        this.host = host;
    }

    /**
     * get url.
     *
     * @return url
     */
    public String getUrl() {
        return url;
    }

    /**
     * set url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
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
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        WebSocketUpstream that = (WebSocketUpstream) o;
        return Objects.equals(host, that.host) && Objects.equals(getProtocol(), that.getProtocol()) && Objects.equals(url, that.url);
    }

    @Override
    public int hashCode() {
        return Objects.hash(host, getProtocol(), url);
    }

    @Override
    public String toString() {
        return "DivideUpstream{"
                + "host='"
                + host
                + '\''
                + ", protocol='"
                + getProtocol()
                + '\''
                + ", url='"
                + url
                + '\''
                + ", weight="
                + weight
                + ", status="
                + isStatus()
                + ", timestamp="
                + getTimestamp()
                + ", warmup="
                + warmup
                + '}';
    }
    

    /**
     * class builder.
     */
    public static final class Builder {

        /**
         * host.
         */
        private String host;

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
         * status.
         */
        private boolean status;
        

        /**
         * timestamp.
         */
        private long timestamp;

        /**
         * warmup.
         */
        private int warmup;

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
        public WebSocketUpstream build() {
            return new WebSocketUpstream(this);
        }

        /**
         * build host.
         *
         * @param host host
         * @return this
         */
        public Builder host(final String host) {
            this.host = host;
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
         * build url.
         *
         * @param url url
         * @return this
         */
        public Builder upstreamUrl(final String url) {
            this.url = url;
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
            this.status = status;
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
    }
}
