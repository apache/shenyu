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
 * The type Sofa selector upstream.
 */
public final class SofaUpstream extends CommonUpstream {

    /**
     * registry url is required.
     */
    private String register;

    /**
     * sofa application name is required.
     */
    private String appName;

    /**
     * port.
     */
    private int port;

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
    private SofaUpstream(final Builder builder) {
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
     * get registry.
     *
     * @return registry
     */
    public String getRegister() {
        return register;
    }

    /**
     * set registry.
     *
     * @param registry registry
     */
    public void setRegister(final String registry) {
        this.register = register;
    }

    /**
     * get appName.
     *
     * @return appName
     */
    public String getAppName() {
        return appName;
    }

    /**
     * set appName.
     *
     * @param appName appName
     */
    public void setAppName(final String appName) {
        this.appName = appName;
    }

    /**
     * get port.
     *
     * @return port
     */
    public int getPort() {
        return port;
    }

    /**
     * set port.
     *
     * @param port port
     */
    public void setPort(final int port) {
        this.port = port;
    }

    /**
     * Gets the value of weight.
     *
     * @return the value of weight
     */
    public int getWeight() {
        return weight;
    }

    /**
     * Sets the weight.
     *
     * @param weight weight
     */
    public void setWeight(final int weight) {
        this.weight = weight;
    }

    /**
     * Gets the value of warmup.
     *
     * @return the value of warmup
     */
    public int getWarmup() {
        return warmup;
    }

    /**
     * Sets the warmup.
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
        if (!(o instanceof SofaUpstream)) {
            return false;
        }
        SofaUpstream that = (SofaUpstream) o;
        return port == that.port
                && Objects.equals(register, that.register)
                && Objects.equals(appName, that.appName)
                && Objects.equals(this.getProtocol(), that.getProtocol())
                && Objects.equals(this.getUpstreamUrl(), that.getUpstreamUrl())
                && Objects.equals(this.isGray(), that.isGray());
    }

    @Override
    public int hashCode() {
        return Objects.hash(register, appName, port, this.isGray());
    }

    @Override
    public String toString() {
        return "SofaUpstream{"
                + "registry='" + register
                + "', appName='" + appName
                + "', protocol='" + this.getProtocol()
                + "', port=" + port
                + ", upstreamUrl='" + this.getUpstreamUrl()
                + "', gray=" + this.isGray()
                + ", weight=" + weight
                + ", warmup=" + warmup
                + ", status=" + isStatus()
                + ", timestamp=" + getTimestamp()
                + "'}";
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
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return DivideUpstream
         */
        public SofaUpstream build() {
            return new SofaUpstream(this);
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

    }
}
