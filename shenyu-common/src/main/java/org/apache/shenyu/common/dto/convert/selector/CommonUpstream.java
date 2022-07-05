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
 * this is common upstream.
 */
public class CommonUpstream {

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
     * false close/ true open.
     */
    private boolean status = true;

    /**
     * startup time.
     */
    private long timestamp;

    /**
     * Instantiates a new Common upstream.
     */
    public CommonUpstream() {

    }

    /**
     * Instantiates a new Common upstream.
     *
     * @param protocol the protocol
     * @param upstreamHost the upstream host
     * @param upstreamUrl the upstream url
     * @param status the upstream status
     * @param timestamp the upstream timestamp
     */
    public CommonUpstream(final String protocol, final String upstreamHost, final String upstreamUrl, final boolean status, final long timestamp) {
        this.protocol = protocol;
        this.upstreamHost = upstreamHost;
        this.upstreamUrl = upstreamUrl;
        this.status = status;
        this.timestamp = timestamp;
    }

    /**
     * get upstreamHost.
     *
     * @return upstreamHost upstream host
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
     * @return protocol protocol
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
     * @return upstreamUrl upstream url
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
     * get status.
     *
     * @return status boolean
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
     * @return timestamp timestamp
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
     * Default status boolean.
     *
     * @return the boolean
     */
    protected static boolean defaultStatus() {
        return true;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        CommonUpstream that = (CommonUpstream) o;
        return Objects.equals(upstreamHost, that.upstreamHost) && Objects.equals(protocol, that.protocol) && Objects.equals(upstreamUrl, that.upstreamUrl);
    }

    @Override
    public int hashCode() {
        return Objects.hash(upstreamHost, protocol, upstreamUrl);
    }

    @Override
    public String toString() {
        return "CommonUpstream{"
                + "protocol='"
                + protocol
                + '\''
                + ", upstreamHost='"
                + upstreamHost
                + '\''
                + ", upstreamUrl='"
                + upstreamUrl
                + '\''
                + ", status="
                + status
                + ", timestamp="
                + timestamp
                + '}';
    }
}
