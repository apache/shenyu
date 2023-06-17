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

    private String name;

    private String type;

    private String level;

    private String serverList;

    private String pluginName;

    private String props;

    public DiscoveryDO() {

    }

    public DiscoveryDO(final String name, final String type, final String level, final String serverList,
                       final String pluginName, final String props) {
        this.name = name;
        this.type = type;
        this.level = level;
        this.serverList = serverList;
        this.pluginName = pluginName;
        this.props = props;
    }

    public DiscoveryDO(final String id, final Timestamp dateCreated, final Timestamp dateUpdated, final String name,
                       final String type, final String level, final String serverList, final String pluginName,
                       final String props) {
        super(id, dateCreated, dateUpdated);
        this.name = name;
        this.type = type;
        this.level = level;
        this.serverList = serverList;
        this.pluginName = pluginName;
        this.props = props;
    }

    /**
     * get the name value.
     * @return the name value
     */
    public String getName() {
        return name;
    }

    /**
     * set the name value.
     * @param name the name value
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get the type value.
     * @return the type value
     */
    public String getType() {
        return type;
    }

    /**
     * set the type value.
     * @param type the type value
     */
    public void setType(final String type) {
        this.type = type;
    }

    /**
     * get the level.
     * @return the level.
     */
    public String getLevel() {
        return level;
    }

    /**
     * set the level.
     * @param level the level.
     */
    public void setLevel(final String level) {
        this.level = level;
    }

    /**
     * get the server list value.
     * @return the server list value
     */
    public String getServerList() {
        return serverList;
    }

    /**
     * set the server list.
     * @param serverList the server list
     */
    public void setServerList(final String serverList) {
        this.serverList = serverList;
    }

    /**
     * get pluginName value.
     * @return pluginName value
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * set pluginName value.
     * @param pluginName pluginName value
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * get props value.
     * @return props value
     */
    public String getProps() {
        return props;
    }

    /**
     * set props value.
     * @param props props value
     */
    public void setProps(final String props) {
        this.props = props;
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
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        DiscoveryDO discoveryDO = (DiscoveryDO) o;
        return Objects.equals(name, discoveryDO.name)
                && Objects.equals(type, discoveryDO.type)
                && Objects.equals(level, discoveryDO.level)
                && Objects.equals(serverList, discoveryDO.serverList)
                && Objects.equals(pluginName, discoveryDO.pluginName)
                && Objects.equals(props, discoveryDO.props);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), name, type, level, serverList, pluginName, props);
    }

    public static final class DiscoveryDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String name;

        private String type;

        private String level;

        private String serverList;

        private String pluginName;

        private String props;

        private DiscoveryDOBuilder() {

        }

        /**
         * id.
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
        public DiscoveryDOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * type.
         *
         * @param type the type.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder type(final String type) {
            this.type = type;
            return this;
        }

        /**
         * level.
         *
         * @param level the level.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder level(final String level) {
            this.level = level;
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
         * build DiscoveryDO.
         * @return DiscoveryDO
         */
        public DiscoveryDO build() {
            DiscoveryDO discoveryDO = new DiscoveryDO();
            discoveryDO.setId(id);
            discoveryDO.setDateCreated(dateCreated);
            discoveryDO.setDateUpdated(dateUpdated);
            discoveryDO.setName(name);
            discoveryDO.setType(type);
            discoveryDO.setLevel(level);
            discoveryDO.setServerList(serverList);
            discoveryDO.setPluginName(pluginName);
            discoveryDO.setProps(props);
            return discoveryDO;
        }
    }
}
