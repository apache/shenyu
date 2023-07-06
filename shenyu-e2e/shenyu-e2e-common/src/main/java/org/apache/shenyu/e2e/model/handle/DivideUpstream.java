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

/**
 * Divide upstream.
 */
public class DivideUpstream {

    /**
     * this is http protocol.
     */
    private String protocol;

    /**
     * url.
     */
    private String upstreamUrl;

    /**
     * false close/ true open.
     */
    private boolean status;

    /**
     * startup time.
     */
    private long timestamp;

    /**
     * weight.
     */
    private int weight;

    /**
     * warmup.
     */
    private int warmup;
    
    /**
     * upstreamHost.
     */
    private String upstreamHost;
    
    private DivideUpstream() {
    }
    
    public DivideUpstream(final Builder builder) {
        this.protocol = builder.protocol;
        this.upstreamUrl = builder.upstreamUrl;
        this.timestamp = builder.timestamp;
        this.weight = builder.weight;
        this.warmup = builder.warmup;
        this.status = builder.status;
        this.upstreamHost = builder.upstreamHost;
    }
    
    /**
     * builder.
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
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
     * @param upstreamUrl upstreamUrl
     */
    public void setUpstreamUrl(final String upstreamUrl) {
        this.upstreamUrl = upstreamUrl;
    }
    
    /**
     * get timestamp.
     * @return timestamp
     */
    public long getTimestamp() {
        return timestamp;
    }

    /**
     * set timestamp.
     * @param timestamp timestamp
     */
    public void setTimestamp(final long timestamp) {
        this.timestamp = timestamp;
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
     * get host of a upstream.
     * @return String
     */
    public String getUpstreamHost() {
        return upstreamHost;
    }
    
    /**
     * set host of a upstream.
     * @param upstreamHost upstreamHost
     */
    public void setUpstreamHost(final String upstreamHost) {
        this.upstreamHost = upstreamHost;
    }
    
    /**
     * class builder.
     */
    public static final class Builder {
        
        /**
         * this is http protocol.
         */
        private String protocol;

        /**
         * url.
         */
        private String upstreamUrl;

        /**
         * startup time.
         */
        private long timestamp;

        /**
         * weight.
         */
        private int weight;

        /**
         * warmup.
         */
        private int warmup;
        
        private boolean status;
        
        /**
         * upstreamHost.
         */
        private String upstreamHost;
        
        public Builder() {
        }
        
        /**
         * build.
         * @return DivideUpstream
         */
        public DivideUpstream build() {
            return new DivideUpstream(this);
        }
        
        /**
         * build protocol.
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
         * build status.
         * @param status status
         * @return this
         */
        public Builder status(final boolean status) {
            this.status = status;
            return this;
        }
        
        /**
         * set host of a upstream builder.
         * @param upstreamHost upstreamHost
         * @return Builder
         */
        public Builder upstreamHost(final String upstreamHost) {
            this.upstreamHost = upstreamHost;
            return this;
        }
    }
}
