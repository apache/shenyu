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

    private String pluginName;

    private String discoveryHandlerId;

    private String selectorId;

    private String proxySelectorId;

    public DiscoveryRelDO() {

    }

    public DiscoveryRelDO(final String pluginName, final String discoveryHandlerId, final String selectorId, final String proxySelectorId) {
        this.pluginName = pluginName;
        this.discoveryHandlerId = discoveryHandlerId;
        this.selectorId = selectorId;
        this.proxySelectorId = proxySelectorId;
    }

    public DiscoveryRelDO(final String id, final Timestamp dateCreated, final Timestamp dateUpdated, final String pluginName,
                          final String discoveryHandlerId, final String selectorId, final String proxySelectorId) {
        super(id, dateCreated, dateUpdated);
        this.pluginName = pluginName;
        this.discoveryHandlerId = discoveryHandlerId;
        this.selectorId = selectorId;
        this.proxySelectorId = proxySelectorId;
    }

    /**
     * get pluginName value.
     * @return pluginName value.
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * set pluginName value.
     * @param pluginName pluginName value.
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * get discovery handler id.
     * @return discovery handler id.
     */
    public String getDiscoveryHandlerId() {
        return discoveryHandlerId;
    }

    /**
     * set discovery handler id.
     * @param discoveryHandlerId discovery handler id.
     */
    public void setDiscoveryHandlerId(final String discoveryHandlerId) {
        this.discoveryHandlerId = discoveryHandlerId;
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
     * @param selectorId service id.
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }

    /**
     * get proxy selector id.
     * @return proxy selector id.
     */
    public String getProxySelectorId() {
        return proxySelectorId;
    }

    /**
     * set proxy selector id.
     * @param proxySelectorId proxy selector id
     */
    public void setProxySelectorId(final String proxySelectorId) {
        this.proxySelectorId = proxySelectorId;
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
        return Objects.equals(pluginName, that.pluginName)
                && Objects.equals(discoveryHandlerId, that.discoveryHandlerId)
                && Objects.equals(selectorId, that.selectorId)
                && Objects.equals(proxySelectorId, that.proxySelectorId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), pluginName, discoveryHandlerId, selectorId, proxySelectorId);
    }

    public static final class DiscoveryRelDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String pluginName;

        private String discoveryHandlerId;

        private String selectorId;

        private String proxySelectorId;

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
         * @param pluginName the plugin name.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryRelDOBuilder pluginName(final String pluginName) {
            this.pluginName = pluginName;
            return this;
        }

        /**
         * discoveryId.
         *
         * @param discoveryHandlerId the discovery handler id.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryRelDOBuilder discoveryHandlerId(final String discoveryHandlerId) {
            this.discoveryHandlerId = discoveryHandlerId;
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

        /**
         * proxySelectorId.
         * @param proxySelectorId the proxySelectorId.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryRelDOBuilder proxySelectorId(final String proxySelectorId) {
            this.proxySelectorId = proxySelectorId;
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
            discoveryRelDO.setPluginName(pluginName);
            discoveryRelDO.setDiscoveryHandlerId(discoveryHandlerId);
            discoveryRelDO.setSelectorId(selectorId);
            discoveryRelDO.setProxySelectorId(proxySelectorId);
            return discoveryRelDO;
        }
    }
}
