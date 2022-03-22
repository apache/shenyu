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
 * The type Dubbo selector upstream.
 */
public final class DubboUpstream extends CommonUpstream {

    /**
     * zookeeper url is required.
     */
    private String registry;

    /**
     * dubbo application name is required.
     */
    private String appName;
    
    /**
     * port.
     */
    private int port;

    /**
     * gray status.
     */
    private Boolean gray;

    /**
     * group.
     */
    private String group;

    /**
     * version.
     */
    private String version;

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
    private DubboUpstream(final Builder builder) {
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
    public String getRegistry() {
        return registry;
    }

    /**
     * set registry.
     *
     * @param registry registry
     */
    public void setRegistry(final String registry) {
        this.registry = registry;
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
     * Gets the value of gray.
     *
     * @return the value of gray
     */
    public Boolean isGray() {
        return gray;
    }

    /**
     * Sets the gray.
     *
     * @param gray gray
     */
    public void setGray(final Boolean gray) {
        this.gray = gray;
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

    /**
     * Gets the value of group.
     *
     * @return the value of group
     */
    public String getGroup() {
        return group;
    }

    /**
     * Sets the group.
     *
     * @param group group
     */
    public void setGroup(final String group) {
        this.group = group;
    }

    /**
     * Gets the value of version.
     *
     * @return the value of version
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets the version.
     *
     * @param version version
     */
    public void setVersion(final String version) {
        this.version = version;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof DubboUpstream)) {
            return false;
        }
        DubboUpstream that = (DubboUpstream) o;
        return port == that.port
                && weight == that.weight
                && warmup == that.warmup
                && Objects.equals(registry, that.registry)
                && Objects.equals(appName, that.appName)
                && Objects.equals(this.getProtocol(), that.getProtocol())
                && Objects.equals(this.getUpstreamUrl(), that.getUpstreamUrl())
                && Objects.equals(gray, that.gray)
                && Objects.equals(group, that.group)
                && Objects.equals(version, that.version);
    }

    @Override
    public int hashCode() {
        return Objects.hash(registry, appName, port, gray, weight, warmup, group, version);
    }

    @Override
    public String toString() {
        return "DubboUpstream{"
                + "registry='" + registry
                + "', appName='" + appName
                + "', protocol='" + this.getProtocol()
                + "', port=" + port
                + ", upstreamUrl='" + this.getUpstreamUrl()
                + "', gray=" + gray
                + ", weight=" + weight
                + ", warmup=" + warmup
                + ", status=" + isStatus()
                + ", timestamp=" + getTimestamp()
                + ", group='" + group
                + ", version='" + version
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
        public DubboUpstream build() {
            return new DubboUpstream(this);
        }
        
        /**
         * build upstreamHost.
         *
         * @param upstreamHost upstreamHost
         * @return this
         */
        public DubboUpstream.Builder upstreamHost(final String upstreamHost) {
            this.upstreamHost = upstreamHost;
            return this;
        }
        
        /**
         * build protocol.
         *
         * @param protocol protocol
         * @return this
         */
        public DubboUpstream.Builder protocol(final String protocol) {
            this.protocol = protocol;
            return this;
        }
        
        /**
         * build upstreamUrl.
         *
         * @param upstreamUrl upstreamUrl
         * @return this
         */
        public DubboUpstream.Builder upstreamUrl(final String upstreamUrl) {
            this.upstreamUrl = upstreamUrl;
            return this;
        }
        
        /**
         * build weight.
         *
         * @param weight weight
         * @return this
         */
        public DubboUpstream.Builder weight(final int weight) {
            this.weight = weight;
            return this;
        }
        
        /**
         * build status.
         *
         * @param status status
         * @return this
         */
        public DubboUpstream.Builder status(final boolean status) {
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
        public DubboUpstream.Builder timestamp(final long timestamp) {
            this.timestamp = timestamp;
            return this;
        }
        
        /**
         * build warmup.
         *
         * @param warmup warmup
         * @return this
         */
        public DubboUpstream.Builder warmup(final int warmup) {
            this.warmup = warmup;
            return this;
        }
    }
}
