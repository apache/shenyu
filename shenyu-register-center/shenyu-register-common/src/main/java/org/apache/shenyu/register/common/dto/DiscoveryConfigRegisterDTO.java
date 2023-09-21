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

package org.apache.shenyu.register.common.dto;

import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.Properties;

public class DiscoveryConfigRegisterDTO implements DataTypeParent {
    private String name;

    private String discoveryType;

    private String serverList;

    private String pluginName;

    private Properties props;

    public DiscoveryConfigRegisterDTO() {
    }

    public DiscoveryConfigRegisterDTO(final String name, final String discoveryType, final String serverList, final String pluginName, final Properties props) {
        this.name = name;
        this.discoveryType = discoveryType;
        this.serverList = serverList;
        this.props = props;
        this.pluginName = pluginName;
    }

    private DiscoveryConfigRegisterDTO(final Builder builder) {
        name = builder.name;
        discoveryType = builder.discoveryType;
        serverList = builder.serverList;
        props = builder.props;
        pluginName = builder.pluginName;
    }

    @Override
    public DataType getType() {
        return DataType.DISCOVERY_CONFIG;
    }

    /**
     * return builder.
     *
     * @return Builder builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * getName.
     *
     * @return String discovery names
     */
    public String getName() {
        return name;
    }

    /**
     * setName.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * getDiscoveryType.
     *
     * @return discoveryType
     */
    public String getDiscoveryType() {
        return discoveryType;
    }

    /**
     * setDiscoveryType.
     *
     * @param discoveryType discoveryType
     */
    public void setDiscoveryType(final String discoveryType) {
        this.discoveryType = discoveryType;
    }

    /**
     * getServerList.
     *
     * @return String serverList
     */
    public String getServerList() {
        return serverList;
    }

    /**
     * setServerList.
     *
     * @param serverList serverList
     */
    public void setServerList(final String serverList) {
        this.serverList = serverList;
    }

    /**
     * getProps.
     *
     * @return Properties props
     */
    public Properties getProps() {
        return props;
    }

    /**
     * setProps.
     *
     * @param props props
     */
    public void setProps(final Properties props) {
        this.props = props;
    }

    /**
     * getPluginName.
     *
     * @return pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * setPluginName.
     *
     * @param pluginName pluginName
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    public static final class Builder {

        private String name;

        private String discoveryType;

        private String serverList;

        private String pluginName;

        private Properties props;

        private Builder() {
        }

        /**
         * name.
         *
         * @param name name
         * @return Builder builder
         */
        public Builder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * discoveryType.
         *
         * @param discoveryType discoveryType
         * @return Builder builder
         */
        public Builder discoveryType(final String discoveryType) {
            this.discoveryType = discoveryType;
            return this;
        }

        /**
         * serverList.
         *
         * @param serverList serverList
         * @return Builder builder
         */
        public Builder serverList(final String serverList) {
            this.serverList = serverList;
            return this;
        }

        /**
         * props.
         *
         * @param props props
         * @return Builder builder
         */
        public Builder props(final Properties props) {
            this.props = props;
            return this;
        }

        /**
         * pluginName.
         *
         * @param pluginName pluginName
         * @return Builder
         */
        public Builder pluginName(final String pluginName) {
            this.pluginName = pluginName;
            return this;
        }

        /**
         * build.
         *
         * @return DiscoveryConfigRegisterDTO discovery config register dto
         */
        public DiscoveryConfigRegisterDTO build() {
            return new DiscoveryConfigRegisterDTO(this);
        }

    }
}
