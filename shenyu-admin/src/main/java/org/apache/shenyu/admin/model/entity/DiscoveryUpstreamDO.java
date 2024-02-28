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

package org.apache.shenyu.admin.model.entity;

import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.util.StringUtils;

import java.sql.Timestamp;
import java.util.Optional;

/**
 * discovery upstream do.
 */
public class DiscoveryUpstreamDO extends BaseDO {

    private static final long serialVersionUID = 4636503463949130337L;

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
     * DiscoveryUpstreamDO.
     */
    public DiscoveryUpstreamDO() {

    }

    /**
     * DiscoveryUpstreamDO.
     *
     * @param discoveryHandlerId discoveryHandlerId
     * @param protocol    protocol
     * @param url         url
     * @param status      status
     * @param weight      weight
     * @param props       props
     */
    public DiscoveryUpstreamDO(final String discoveryHandlerId, final String protocol, final String url, final int status,
                               final int weight, final String props) {

        this.discoveryHandlerId = discoveryHandlerId;
        this.protocol = protocol;
        this.url = url;
        this.status = status;
        this.weight = weight;
        this.props = props;
    }

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
     * builder.
     *
     * @return DiscoveryUpstreamBuilder
     */
    public static DiscoveryUpstreamBuilder builder() {

        return new DiscoveryUpstreamBuilder();
    }

    /**
     * buildDiscoveryUpstreamDO.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return DiscoveryUpstreamDO
     */
    public static DiscoveryUpstreamDO buildDiscoveryUpstreamDO(final DiscoveryUpstreamDTO discoveryUpstreamDTO) {

        return Optional.ofNullable(discoveryUpstreamDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.builder()
                    .discoveryHandlerId(item.getDiscoveryHandlerId())
                    .protocol(item.getProtocol())
                    .status(item.getStatus())
                    .weight(item.getWeight())
                    .props(item.getProps())
                    .url(item.getUrl())
                    .dateCreated(currentTime)
                    .dateUpdated(currentTime).build();
            if (StringUtils.hasLength(item.getId())) {
                discoveryUpstreamDO.setId(item.getId());
            } else {
                discoveryUpstreamDO.setId(UUIDUtils.getInstance().generateShortUuid());
                discoveryUpstreamDO.setDateCreated(currentTime);
            }
            return discoveryUpstreamDO;
        }).orElse(null);
    }

    /**
     * DiscoveryUpstreamBuilder.
     */
    public static final class DiscoveryUpstreamBuilder {

        /**
         * id.
         */
        private String id;

        /**
         * dateCreated.
         */
        private Timestamp dateCreated;

        /**
         * dateUpdated.
         */
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
         * id.
         *
         * @param id the id.
         * @return ProxySelectorBuilder.
         */
        public DiscoveryUpstreamBuilder id(final String id) {

            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return ProxySelectorBuilder.
         */
        public DiscoveryUpstreamBuilder dateCreated(final Timestamp dateCreated) {

            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return ProxySelectorBuilder.
         */
        public DiscoveryUpstreamBuilder dateUpdated(final Timestamp dateUpdated) {

            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * discoveryHandlerId.
         *
         * @param discoveryHandlerId discoveryHandlerId
         * @return DiscoveryUpstreamBuilder
         */
        public DiscoveryUpstreamBuilder discoveryHandlerId(final String discoveryHandlerId) {

            this.discoveryHandlerId = discoveryHandlerId;
            return this;
        }

        /**
         * protocol.
         *
         * @param protocol protocol
         * @return DiscoveryUpstreamBuilder
         */
        public DiscoveryUpstreamBuilder protocol(final String protocol) {

            this.protocol = protocol;
            return this;
        }

        /**
         * url.
         *
         * @param url url
         * @return DiscoveryUpstreamBuilder
         */
        public DiscoveryUpstreamBuilder url(final String url) {

            this.url = url;
            return this;
        }

        /**
         * status.
         *
         * @param status status
         * @return DiscoveryUpstreamBuilder
         */
        public DiscoveryUpstreamBuilder status(final int status) {

            this.status = status;
            return this;
        }

        /**
         * weight.
         *
         * @param weight weight
         * @return DiscoveryUpstreamBuilder
         */
        public DiscoveryUpstreamBuilder weight(final int weight) {

            this.weight = weight;
            return this;
        }

        /**
         * props.
         *
         * @param props props
         * @return DiscoveryUpstreamBuilder
         */
        public DiscoveryUpstreamBuilder props(final String props) {

            this.props = props;
            return this;
        }

        /**
         * build.
         *
         * @return DiscoveryUpstreamDO
         */
        public DiscoveryUpstreamDO build() {

            DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
            discoveryUpstreamDO.setId(this.id);
            discoveryUpstreamDO.setDiscoveryHandlerId(this.discoveryHandlerId);
            discoveryUpstreamDO.setProtocol(this.protocol);
            discoveryUpstreamDO.setUrl(this.url);
            discoveryUpstreamDO.setStatus(this.status);
            discoveryUpstreamDO.setWeight(this.weight);
            discoveryUpstreamDO.setProps(this.props);
            discoveryUpstreamDO.setDateCreated(this.dateCreated);
            discoveryUpstreamDO.setDateUpdated(this.dateUpdated);
            return discoveryUpstreamDO;
        }
    }
}
