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

    private String selectorName;

    private String name;

    private String discoveryType;

    private String serverList;

    private String listenerNode;

    private String handler;

    private String pluginName;

    private Properties props;

    private String namespaceId;

    public DiscoveryConfigRegisterDTO() {
    }

    public DiscoveryConfigRegisterDTO(final String selectorName, final String name, final String discoveryType, final String serverList,
                                      final String listenerNode, final String handler, final String pluginName, final Properties props) {
        this.selectorName = selectorName;
        this.name = name;
        this.discoveryType = discoveryType;
        this.serverList = serverList;
        this.listenerNode = listenerNode;
        this.handler = handler;
        this.props = props;
        this.pluginName = pluginName;
    }

    private DiscoveryConfigRegisterDTO(final Builder builder) {
        selectorName = builder.selectorName;
        name = builder.name;
        discoveryType = builder.discoveryType;
        serverList = builder.serverList;
        props = builder.props;
        pluginName = builder.pluginName;
        listenerNode = builder.listenerNode;
        handler = builder.handler;
        namespaceId = builder.namespaceId;
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
     * getSelectorName.
     *
     * @return selectorName
     */
    public String getSelectorName() {
        return selectorName;
    }

    /**
     * setSelectorName.
     *
     * @param selectorName selectorName
     */
    public void setSelectorName(final String selectorName) {
        this.selectorName = selectorName;
    }

    /**
     * getHandler.
     *
     * @return handler
     */
    public String getHandler() {
        return handler;
    }

    /**
     * setHandler.
     *
     * @param handler handler
     */
    public void setHandler(final String handler) {
        this.handler = handler;
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

    /**
     * getListenerNode.
     *
     * @return listenerNode
     */
    public String getListenerNode() {
        return listenerNode;
    }

    /**
     * setListenerNode.
     *
     * @param listenerNode listenerNode
     */
    public void setListenerNode(final String listenerNode) {
        this.listenerNode = listenerNode;
    }

    /**
     * Gets the value of namespaceId.
     *
     * @return the value of namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * Sets the namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    public static final class Builder {

        private String selectorName;

        private String name;

        private String discoveryType;

        private String serverList;

        private String pluginName;

        private String listenerNode;

        private String handler;

        private Properties props;

        private String namespaceId;

        private Builder() {
        }


        /**
         * selectorName.
         *
         * @param selectorName selectorName
         * @return Builder builder
         */
        public Builder selectorName(final String selectorName) {
            this.selectorName = selectorName;
            return this;
        }


        /**
         * listenerNode.
         *
         * @param listenerNode listenerNode
         * @return Builder builder
         */
        public Builder listenerNode(final String listenerNode) {
            this.listenerNode = listenerNode;
            return this;
        }

        /**
         * handler.
         *
         * @param handler handler
         * @return Builder builder
         */
        public Builder handler(final String handler) {
            this.handler = handler;
            return this;
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
         * namespaceId.
         *
         * @param namespaceId namespaceId
         * @return SelectorDOBuilder
         */
        public Builder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
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
