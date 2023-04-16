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
 * DiscoveryRelDO
 */
public final class DiscoveryRelDO extends BaseDO {

    private String discoveryId;

    private String selectorId;

    public DiscoveryRelDO() {}

    public DiscoveryRelDO(String discoveryId, String selectorId) {
        this.discoveryId = discoveryId;
        this.selectorId = selectorId;
    }

    public DiscoveryRelDO(String id, Timestamp dateCreated, Timestamp dateUpdated, String discoveryId, String selectorId) {
        super(id, dateCreated, dateUpdated);
        this.discoveryId = discoveryId;
        this.selectorId = selectorId;
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
    public void setDiscoveryId(String discoveryId) {
        this.discoveryId = discoveryId;
    }

    /**
     * get selector id.
     * @return selector id.
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * set selector id.
     * @param selectorId selector id.
     */
    public void setSelectorId(String selectorId) {
        this.selectorId = selectorId;
    }

    @Override
    public boolean equals(Object o) {
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
        return Objects.equals(discoveryId, that.discoveryId) && Objects.equals(selectorId, that.selectorId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), discoveryId, selectorId);
    }

    public static final class DiscoveryRelDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String discoveryId;

        private String selectorId;

        private DiscoveryRelDOBuilder() {}

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
         * @param selectorId the selectorId.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryRelDOBuilder selectorId(final String selectorId) {
            this.selectorId = selectorId;
            return this;
        }

        public DiscoveryRelDO build() {
            DiscoveryRelDO discoveryRelDO = new DiscoveryRelDO();
            discoveryRelDO.setId(id);
            discoveryRelDO.setDateCreated(dateCreated);
            discoveryRelDO.setDateUpdated(dateUpdated);
            discoveryRelDO.setDiscoveryId(discoveryId);
            discoveryRelDO.setSelectorId(selectorId);
            return discoveryRelDO;
        }
    }
}
