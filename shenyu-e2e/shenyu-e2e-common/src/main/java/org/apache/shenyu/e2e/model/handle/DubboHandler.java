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

    public static Builder builder() {
        return new Builder();
    }

    public DubboHandler(Builder builder) {
        this.upstreamHost = builder.upstreamHost;
        this.protocol = builder.protocol;
        this.upstreamUrl = builder.upstreamUrl;
        this.gray = builder.gray;
        this.status = builder.status;
        this.timestamp = builder.timestamp;
        this.warmup = builder.warmup;
        this.weight = builder.weight;
    }

    public String getUpstreamHost() {
        return upstreamHost;
    }

    public void setUpstreamHost(String upstreamHost) {
        this.upstreamHost = upstreamHost;
    }

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    public String getUpstreamUrl() {
        return upstreamUrl;
    }

    public void setUpstreamUrl(String upstreamUrl) {
        this.upstreamUrl = upstreamUrl;
    }

    public String getGray() {
        return gray;
    }

    public void setGray(String gray) {
        this.gray = gray;
    }

    public boolean isStatus() {
        return status;
    }

    public void setStatus(boolean status) {
        this.status = status;
    }

    public long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(long timestamp) {
        this.timestamp = timestamp;
    }

    public long getWarmup() {
        return warmup;
    }

    public void setWarmup(long warmup) {
        this.warmup = warmup;
    }

    public int getWeight() {
        return weight;
    }

    public void setWeight(int weight) {
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

        public DubboHandler build() {
            return new DubboHandler(this);
        }

        public Builder upstreamHost(String upstreamHost) {
            this.upstreamHost = upstreamHost;
            return this;
        }

        public Builder protocol(String protocol) {
            this.protocol = protocol;
            return this;
        }

        public Builder upstreamUrl(String upstreamUrl) {
            this.upstreamUrl = upstreamUrl;
            return this;
        }

        public Builder gray(String gray) {
            this.gray = gray;
            return this;
        }

        public Builder status(boolean status) {
            this.status = status;
            return this;
        }

        public Builder timestamp(long timestamp) {
            this.timestamp = timestamp;
            return this;
        }

        public Builder warmup(long warmup) {
            this.warmup = warmup;
            return this;
        }

        public Builder weight(int weight) {
            this.weight = weight;
            return this;
        }
    }
}
