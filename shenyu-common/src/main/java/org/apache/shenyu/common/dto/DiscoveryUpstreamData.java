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

package org.apache.shenyu.common.dto;

import com.fasterxml.jackson.annotation.JsonFormat;

import java.sql.Timestamp;
import java.util.Objects;

public class DiscoveryUpstreamData {

    /**
     * primary key.
     */
    private String id;

    /**
     * created time.
     */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Timestamp dateCreated;

    /**
     * updated time.
     */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Timestamp dateUpdated;


    /**
     * discoveryHandlerId.
     */
    private String discoveryHandlerId;

    /**
     * protocol.
     */
    private String protocol;

    /**
     * url.
     */
    private String url;

    /**
     * status.
     */
    private int status;

    /**
     * weight.
     */
    private int weight;

    /**
     * props.
     */
    private String props;

    /**
     * namespaceId.
     */
    private String namespaceId;


    /**
     * getDiscoveryHandlerId.
     *
     * @return discoveryHandlerId
     */
    public String getDiscoveryHandlerId() {
        return discoveryHandlerId;
    }

    /**
     * setDiscoveryHandlerId.
     *
     * @param discoveryHandlerId discoveryHandlerId
     */
    public void setDiscoveryHandlerId(final String discoveryHandlerId) {
        this.discoveryHandlerId = discoveryHandlerId;
    }

    /**
     * getProtocol.
     *
     * @return protocol
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * setProtocol.
     *
     * @param protocol protocol
     */
    public void setProtocol(final String protocol) {
        this.protocol = protocol;
    }

    /**
     * getUrl.
     *
     * @return url
     */
    public String getUrl() {
        return url;
    }

    /**
     * setUrl.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * getStatus.
     *
     * @return status
     */
    public int getStatus() {
        return status;
    }

    /**
     * setStatus.
     *
     * @param status status
     */
    public void setStatus(final int status) {
        this.status = status;
    }

    /**
     * getWeight.
     *
     * @return weight
     */
    public int getWeight() {
        return weight;
    }

    /**
     * setWeight.
     *
     * @param weight weight
     */
    public void setWeight(final int weight) {
        this.weight = weight;
    }

    /**
     * getProps.
     *
     * @return props
     */
    public String getProps() {
        return props;
    }

    /**
     * setProps.
     *
     * @param props props
     */
    public void setProps(final String props) {
        this.props = props;
    }

    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * Gets the value of dateCreated.
     *
     * @return the value of dateCreated
     */
    public Timestamp getDateCreated() {
        return dateCreated;
    }

    /**
     * Sets the dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Timestamp dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * Gets the value of dateUpdated.
     *
     * @return the value of dateUpdated
     */
    public Timestamp getDateUpdated() {
        return dateUpdated;
    }

    /**
     * setDateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Timestamp dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    /**
     * get namespaceId.
     *
     * @return namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        DiscoveryUpstreamData that = (DiscoveryUpstreamData) o;
        return status == that.status && weight == that.weight && Objects.equals(id, that.id)
                && Objects.equals(dateCreated, that.dateCreated) && Objects.equals(dateUpdated, that.dateUpdated)
                && Objects.equals(discoveryHandlerId, that.discoveryHandlerId) && Objects.equals(protocol, that.protocol)
                && Objects.equals(url, that.url) && Objects.equals(props, that.props)
                && Objects.equals(namespaceId, that.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, dateCreated, dateUpdated, discoveryHandlerId, protocol, url, status, weight, props, namespaceId);
    }

    /**
     * class builder.
     *
     * @return Builder
     */
    public static DiscoveryUpstreamData.Builder builder() {
        return new DiscoveryUpstreamData.Builder();
    }

    public static final class Builder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String discoveryHandlerId;

        private String protocol;

        private String url;

        private int status;

        private int weight;

        private String props;

        private String namespaceId;

        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return Builder
         */
        public static Builder builder() {
            return new Builder();
        }

        /**
         * build id.
         *
         * @param id id
         * @return this
         */
        public Builder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * build dateCreated.
         *
         * @param dateCreated dateCreated
         * @return this
         */
        public Builder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * build dateUpdated.
         *
         * @param dateUpdated dateUpdated
         * @return this
         */
        public Builder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build discoveryHandlerId.
         *
         * @param discoveryHandlerId discoveryHandlerId
         * @return this
         */
        public Builder discoveryHandlerId(final String discoveryHandlerId) {
            this.discoveryHandlerId = discoveryHandlerId;
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
        public Builder url(final String url) {
            this.url = url;
            return this;
        }

        /**
         * build status.
         *
         * @param status status
         * @return this
         */
        public Builder status(final int status) {
            this.status = status;
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
         * build props.
         *
         * @param props props
         * @return this
         */
        public Builder props(final String props) {
            this.props = props;
            return this;
        }

        /**
         * build namespaceId.
         *
         * @param namespaceId namespaceId
         * @return this
         */
        public Builder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        /**
         * build new Object.
         *
         * @return DiscoveryUpstreamData
         */
        public DiscoveryUpstreamData build() {
            DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
            discoveryUpstreamData.setId(id);
            discoveryUpstreamData.setDateCreated(dateCreated);
            discoveryUpstreamData.setDateUpdated(dateUpdated);
            discoveryUpstreamData.setDiscoveryHandlerId(discoveryHandlerId);
            discoveryUpstreamData.setProtocol(protocol);
            discoveryUpstreamData.setUrl(url);
            discoveryUpstreamData.setStatus(status);
            discoveryUpstreamData.setWeight(weight);
            discoveryUpstreamData.setProps(props);
            discoveryUpstreamData.setNamespaceId(namespaceId);
            return discoveryUpstreamData;
        }
    }
}
