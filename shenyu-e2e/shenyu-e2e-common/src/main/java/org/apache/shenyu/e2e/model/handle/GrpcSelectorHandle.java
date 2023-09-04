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
 * SpringCLoud selector handle.
 */
public class GrpcSelectorHandle implements PluginHandle {

    /**
     * this is register eureka serviceId.
     */
    private String upstreamUrl;

    /**
     * weight.
     */
    private Integer weight;

    /**
     * status.
     */
    private Boolean status;

    /**
     * no args constructor.
     */
    public GrpcSelectorHandle() {
    }

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    public GrpcSelectorHandle(final GrpcSelectorHandle.Builder builder) {
        this.upstreamUrl = builder.upstreamUrl;
        this.weight = builder.weight;
        this.status = builder.status;
    }

    /**
     * builder.
     *
     * @return Builder
     */
    public static GrpcSelectorHandle.Builder builder() {
        return new GrpcSelectorHandle.Builder();
    }

    /**
     * get upstreamUrl.
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
     * get weight.
     * @return weight
     */
    public Integer getWeight() {
        return weight;
    }

    /**
     * set weight.
     * @param weight weight
     */
    public void setWeight(final Integer weight) {
        this.weight = weight;
    }

    /**
     * get status.
     * @return status
     */
    public Boolean getStatus() {
        return status;
    }

    /**
     * set status.
     * @param status status
     */
    public void setStatus(final Boolean status) {
        this.status = status;
    }

    /**
     * builder class.
     */
    public static final class Builder {
        private String upstreamUrl;

        private Integer weight;

        private Boolean status;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build.
         *
         * @return SpringCloudRuleHandle
         */
        public GrpcSelectorHandle build() {
            return new GrpcSelectorHandle(this);
        }

        /**
         * get upstreamUrl.
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
         * get weight.
         * @return weight
         */
        public Integer getWeight() {
            return weight;
        }

        /**
         *set weight.
         * @param weight weight
         */
        public void setWeight(final Integer weight) {
            this.weight = weight;
        }

        /**
         * get status.
         * @return status
         */
        public Boolean getStatus() {
            return status;
        }

        /**
         * set status.
         * @param status status
         */
        public void setStatus(final Boolean status) {
            this.status = status;
        }

        /**
         * upstreamUrl.
         * @param upstreamUrl upstreamUrl
         * @return Builder
         */
        public GrpcSelectorHandle.Builder upstreamUrl(final String upstreamUrl) {
            this.upstreamUrl = upstreamUrl;
            return this;
        }

        /**
         * status.
         * @param status status
         * @return Builder
         */
        public GrpcSelectorHandle.Builder status(final Boolean status) {
            this.status = status;
            return this;
        }

        /**
         * weight.
         * @param weight weight
         * @return Builder
         */
        public GrpcSelectorHandle.Builder weight(final Integer weight) {
            this.weight = weight;
            return this;
        }

    }
}
