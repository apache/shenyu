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

import org.apache.shenyu.common.dto.convert.DivideUpstream;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * The type Spring cloud selector handle.
 */
public class SpringCloudSelectorHandle implements Serializable {

    private static final long serialVersionUID = -5325946855733519631L;

    /**
     * this is register eureka serviceId.
     */
    private String serviceId;

    /**
     * this is gray.
     */
    private boolean gray;

    /**
     * The divideUpstreams.
     */
    private List<DivideUpstream> divideUpstreams;

    /**
     * no args constructor.
     */
    public SpringCloudSelectorHandle() {
    }

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private SpringCloudSelectorHandle(final Builder builder) {
        this.gray = builder.gray;
        this.serviceId = builder.serviceId;
        this.divideUpstreams = builder.divideUpstreams;
    }

    /**
     * class builder.
     *
     * @return class Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * get serviceId.
     *
     * @return serviceId
     */
    public String getServiceId() {
        return serviceId;
    }

    /**
     * set serviceId.
     *
     * @param serviceId serviceId
     */
    public void setServiceId(final String serviceId) {
        this.serviceId = serviceId;
    }

    /**
     * get gray.
     *
     * @return the boolean
     */
    public boolean getGray() {
        return gray;
    }

    /**
     * set gray.
     *
     * @param gray gray
     */
    public void setGray(final Boolean gray) {
        this.gray = gray;
    }

    /**
     * get divideUpstreams.
     *
     * @return the divideUpstreams
     */
    public List<DivideUpstream> getDivideUpstreams() {
        return divideUpstreams;
    }

    /**
     * set divideUpstreams.
     *
     * @param divideUpstreams divideUpstreams
     */
    public void setDivideUpstreams(final List<DivideUpstream> divideUpstreams) {
        this.divideUpstreams = divideUpstreams;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        SpringCloudSelectorHandle that = (SpringCloudSelectorHandle) o;
        return Objects.equals(serviceId, that.serviceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(serviceId);
    }

    @Override
    public String toString() {
        return "SpringCloudSelectorHandle{"
                + "serviceId='"
                + serviceId
                + '\''
                + '}';
    }

    /**
     * class builder.
     */
    public static final class Builder {
        /**
         * serviceId.
         */
        private String serviceId;

        /**
         * this is gray.
         */
        private boolean gray;

        /**
         * The divideUpstreams.
         */
        private List<DivideUpstream> divideUpstreams;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return SpringCloudSelectorHandle
         */
        public SpringCloudSelectorHandle build() {
            return new SpringCloudSelectorHandle(this);
        }

        /**
         * build serviceId.
         *
         * @param serviceId serviceId
         * @return this
         */
        public Builder serviceId(final String serviceId) {
            this.serviceId = serviceId;
            return this;
        }

        /**
         * build gray.
         *
         * @param gray gray
         * @return this
         */
        public Builder gray(final boolean gray) {
            this.gray = gray;
            return this;
        }

        /**
         * build serviceId.
         *
         * @param divideUpstreams divideUpstreams
         * @return this
         */
        public Builder divideUpstreams(final List<DivideUpstream> divideUpstreams) {
            this.divideUpstreams = divideUpstreams;
            return this;
        }
    }
}
