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
 * DiscoveryDO.
 */
public final class DiscoveryDO extends BaseDO {

    private String discoveryName;

    private String discoveryType;

    private String discoveryLevel;

    private String serverList;

    private String pluginName;

    private String props;

    private String namespaceId;

    public DiscoveryDO() {

    }

    public DiscoveryDO(final String discoveryName, final String discoveryType, final String level, final String serverList,
                       final String pluginName, final String props) {
        this.discoveryName = discoveryName;
        this.discoveryType = discoveryType;
        this.discoveryLevel = discoveryLevel;
        this.serverList = serverList;
        this.pluginName = pluginName;
        this.props = props;
    }

    public DiscoveryDO(final String id, final Timestamp dateCreated, final Timestamp dateUpdated, final String discoveryName,
                       final String discoveryType, final String discoveryLevel, final String serverList, final String pluginName,
                       final String props) {

        super(id, dateCreated, dateUpdated);
        this.discoveryName = discoveryName;
        this.discoveryType = discoveryType;
        this.discoveryLevel = discoveryLevel;
        this.serverList = serverList;
        this.pluginName = pluginName;
        this.props = props;
    }

    /**
     * get the name value.
     *
     * @return the name value
     */
    public String getDiscoveryName() {
        return discoveryName;
    }

    /**
     * set the name value.
     *
     * @param name the name value
     */
    public void setDiscoveryName(final String name) {
        this.discoveryName = name;
    }

    /**
     * get the type value.
     *
     * @return the type value
     */
    public String getDiscoveryType() {
        return discoveryType;
    }

    /**
     * set the type value.
     *
     * @param type the type value
     */
    public void setDiscoveryType(final String type) {
        this.discoveryType = type;
    }

    /**
     * get the discovery level.
     *
     * @return the discovery level.
     */
    public String getDiscoveryLevel() {
        return discoveryLevel;
    }

    /**
     * set the discovery level.
     *
     * @param discoveryLevel the level.
     */
    public void setDiscoveryLevel(final String discoveryLevel) {
        this.discoveryLevel = discoveryLevel;
    }

    /**
     * get the server list value.
     *
     * @return the server list value
     */
    public String getServerList() {
        return serverList;
    }

    /**
     * set the server list.
     *
     * @param serverList the server list
     */
    public void setServerList(final String serverList) {
        this.serverList = serverList;
    }

    /**
     * get pluginName value.
     *
     * @return pluginName value
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * set pluginName value.
     *
     * @param pluginName pluginName value
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * get props value.
     *
     * @return props value
     */
    public String getProps() {
        return props;
    }

    /**
     * set props value.
     *
     * @param props props value
     */
    public void setProps(final String props) {
        this.props = props;
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

    /**
     * builder.
     *
     * @return discoveryDOBuilder
     */
    public static DiscoveryDO.DiscoveryDOBuilder builder() {
        return new DiscoveryDO.DiscoveryDOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        DiscoveryDO discoveryDO = (DiscoveryDO) o;
        return Objects.equals(discoveryName, discoveryDO.discoveryName)
                && Objects.equals(discoveryType, discoveryDO.discoveryType)
                && Objects.equals(discoveryLevel, discoveryDO.discoveryLevel)
                && Objects.equals(serverList, discoveryDO.serverList)
                && Objects.equals(pluginName, discoveryDO.pluginName)
                && Objects.equals(props, discoveryDO.props)
                && Objects.equals(namespaceId, discoveryDO.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), discoveryName, discoveryType, discoveryLevel, serverList, pluginName, props, namespaceId);
    }

    public static final class DiscoveryDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String discoveryName;

        private String discoveryType;

        private String discoveryLevel;

        private String serverList;

        private String pluginName;

        private String props;

        private String namespaceId;

        private DiscoveryDOBuilder() {

        }

        /**
         * id.
         *
         * @param id the id
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * name.
         *
         * @param name the type.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder discoveryName(final String name) {
            this.discoveryName = name;
            return this;
        }

        /**
         * type.
         *
         * @param type the type.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder discoveryType(final String type) {
            this.discoveryType = type;
            return this;
        }

        /**
         * discovery level.
         *
         * @param discoveryLevel the discovery level.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder discoveryLevel(final String discoveryLevel) {
            this.discoveryLevel = discoveryLevel;
            return this;
        }

        /**
         * service list.
         *
         * @param serverList the server list.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder serverList(final String serverList) {
            this.serverList = serverList;
            return this;
        }

        /**
         * pluginName.
         *
         * @param pluginName the pluginName.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder pluginName(final String pluginName) {
            this.pluginName = pluginName;
            return this;
        }

        /**
         * props.
         *
         * @param props the props.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder props(final String props) {
            this.props = props;
            return this;
        }

        /**
         * namespaceId.
         *
         * @param namespaceId namespaceId
         * @return DiscoveryDOBuilder
         */
        public DiscoveryDOBuilder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }


        /**
         * build DiscoveryDO.
         *
         * @return DiscoveryDO
         */
        public DiscoveryDO build() {
            DiscoveryDO discoveryDO = new DiscoveryDO();
            discoveryDO.setId(id);
            discoveryDO.setDateCreated(dateCreated);
            discoveryDO.setDateUpdated(dateUpdated);
            discoveryDO.setDiscoveryName(discoveryName);
            discoveryDO.setDiscoveryType(discoveryType);
            discoveryDO.setDiscoveryLevel(discoveryLevel);
            discoveryDO.setServerList(serverList);
            discoveryDO.setPluginName(pluginName);
            discoveryDO.setProps(props);
            discoveryDO.setNamespaceId(namespaceId);
            return discoveryDO;
        }
    }
}
