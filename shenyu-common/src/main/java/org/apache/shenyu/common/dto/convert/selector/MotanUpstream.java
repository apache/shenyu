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
 * this is motan upstream.
 */
public final class MotanUpstream extends CommonUpstream {

    /**
     * registerAddress.
     */
    private String registerAddress;

    /**
     * registerProtocol.
     */
    private String registerProtocol;

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private MotanUpstream(final Builder builder) {
        boolean statusValue = builder.statusValue;
        if (!builder.statusSet) {
            statusValue = defaultStatus();
        }
        setUpstreamHost(builder.upstreamHost);
        setProtocol(builder.protocol);
        setUpstreamUrl(builder.upstreamUrl);
        setStatus(statusValue);
        setTimestamp(builder.timestamp);
        setRegisterAddress(builder.registerAddress);
        setRegisterProtocol(builder.registerProtocol);
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
     * get registerAddress.
     *
     * @return registerAddress
     */
    public String getRegisterAddress() {
        return registerAddress;
    }

    /**
     * set registerAddress.
     *
     * @param registerAddress registerAddress
     */
    public void setRegisterAddress(final String registerAddress) {
        this.registerAddress = registerAddress;
    }

    /**
     * get registerProtocol.
     *
     * @return registerProtocol
     */
    public String getRegisterProtocol() {
        return registerProtocol;
    }

    /**
     * set registerProtocol.
     *
     * @param registerProtocol registerProtocol
     */
    public void setRegisterProtocol(final String registerProtocol) {
        this.registerProtocol = registerProtocol;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MotanUpstream)) {
            return false;
        }
        MotanUpstream that = (MotanUpstream) o;
        return Objects.equals(registerAddress, that.registerAddress)
                && Objects.equals(this.getProtocol(), that.getProtocol())
                && Objects.equals(this.getUpstreamUrl(), that.getUpstreamUrl())
                && Objects.equals(this.isGray(), that.isGray())
                && Objects.equals(this.registerProtocol, that.registerProtocol);
    }

    @Override
    public int hashCode() {
        return Objects.hash(registerAddress, registerProtocol, this.isGray());
    }

    @Override
    public String toString() {
        return "MotanUpstream{"
                + "registerAddress='" + registerAddress
                + ", registerProtocol=" + registerProtocol
                + "', protocol='" + this.getProtocol()
                + ", upstreamUrl='" + this.getUpstreamUrl()
                + "', gray=" + this.isGray()
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
         * registerAddress.
         */
        private String registerAddress;

        /**
         * registerProtocol.
         */
        private String registerProtocol;

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
        public MotanUpstream build() {
            return new MotanUpstream(this);
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
         * build registerAddress.
         *
         * @param registerAddress registerAddress
         * @return this
         */
        public Builder registerAddress(final String registerAddress) {
            this.registerAddress = registerAddress;
            return this;
        }

        /**
         * build registerAddress.
         *
         * @param registerProtocol registerProtocol
         * @return this
         */
        public Builder registerProtocol(final String registerProtocol) {
            this.registerProtocol = registerProtocol;
            return this;
        }
    }

}
