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

import java.sql.Timestamp;
import java.util.Objects;

/**
 * DiscoveryRelDO.
 */
public final class DiscoveryRelDO extends BaseDO {

    private String level;

    private String discoveryId;

    private String serviceId;

    public DiscoveryRelDO() {

    }

    public DiscoveryRelDO(final String level, final String discoveryId, final String serviceId) {
        this.level = level;
        this.discoveryId = discoveryId;
        this.serviceId = serviceId;
    }

    public DiscoveryRelDO(final String id, final Timestamp dateCreated, final Timestamp dateUpdated, final String level,
                          final String discoveryId, final String serviceId) {
        super(id, dateCreated, dateUpdated);
        this.level = level;
        this.discoveryId = discoveryId;
        this.serviceId = serviceId;
    }

    /**
     * get level value.
     * @return level value.
     */
    public String getLevel() {
        return level;
    }

    /**
     * set level value.
     * @param level level value.
     */
    public void setLevel(final String level) {
        this.level = level;
    }

    /**
     * get discovery id.
     * @return discovery id.
     */
    public String getDiscoveryId() {
        return discoveryId;
    }

    /**
     * set discovery id.
     * @param discoveryId discovery id.
     */
    public void setDiscoveryId(final String discoveryId) {
        this.discoveryId = discoveryId;
    }

    /**
     * get selector id.
     * @return selector id.
     */
    public String getServiceId() {
        return serviceId;
    }

    /**
     * set selector id.
     * @param serviceId service id.
     */
    public void setServiceId(final String serviceId) {
        this.serviceId = serviceId;
    }

    /**
     * builder.
     *
     * @return discoveryRelDOBuilder
     */
    public static DiscoveryRelDO.DiscoveryRelDOBuilder builder() {
        return new DiscoveryRelDO.DiscoveryRelDOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        DiscoveryRelDO that = (DiscoveryRelDO) o;
        return Objects.equals(level, that.level)
                && Objects.equals(discoveryId, that.discoveryId)
                && Objects.equals(serviceId, that.serviceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), level, discoveryId, serviceId);
    }

    public static final class DiscoveryRelDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String level;

        private String discoveryId;

        private String serviceId;

        private DiscoveryRelDOBuilder() {

        }

        /**
         * id.
         *
         * @param id the id.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryRelDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryRelDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryRelDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * level.
         *
         * @param level the level.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryRelDOBuilder level(final String level) {
            this.level = level;
            return this;
        }

        /**
         * discoveryId.
         *
         * @param discoveryId the discoveryId.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryRelDOBuilder discoveryId(final String discoveryId) {
            this.discoveryId = discoveryId;
            return this;
        }

        /**
         * selectorId.
         *
         * @param serviceId the selectorId.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryRelDOBuilder serviceId(final String serviceId) {
            this.serviceId = serviceId;
            return this;
        }

        /**
         * build DiscoveryRelDO.
         * @return DiscoveryRelDO
         */
        public DiscoveryRelDO build() {
            DiscoveryRelDO discoveryRelDO = new DiscoveryRelDO();
            discoveryRelDO.setId(id);
            discoveryRelDO.setDateCreated(dateCreated);
            discoveryRelDO.setDateUpdated(dateUpdated);
            discoveryRelDO.setLevel(level);
            discoveryRelDO.setDiscoveryId(discoveryId);
            discoveryRelDO.setServiceId(serviceId);
            return discoveryRelDO;
        }
    }
}
