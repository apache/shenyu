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

package org.apache.shenyu.e2e.model.data;

import java.util.List;

public class BindingData implements ResourceData {

    private String id;

    private String type;

    private String selectorId;

    private String pluginName;

    private String name;

    private String handler;

    private Discovery discovery;

    private List<DiscoveryUpstream> discoveryUpstreams;

    private String namespaceId;

    @Override
    public String getId() {
        return id;
    }

    /**
     * setId.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * getType.
     *
     * @return type
     */
    public String getType() {
        return type;
    }

    /**
     * setType.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
    }

    /**
     * getSelectorId.
     *
     * @return selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * setSelectorId.
     *
     * @param selectorId selectorId
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
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
     * getName.
     *
     * @return name
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
     * getDiscovery.
     *
     * @return discovery
     */
    public Discovery getDiscovery() {
        return discovery;
    }

    /**
     * setDiscovery.
     *
     * @param discovery discovery
     */
    public void setDiscovery(final Discovery discovery) {
        this.discovery = discovery;
    }

    /**
     * getDiscoveryUpstreams.
     *
     * @return discoveryUpstreams
     */
    public List<DiscoveryUpstream> getDiscoveryUpstreams() {
        return discoveryUpstreams;
    }

    /**
     * setDiscoveryUpstreams.
     *
     * @param discoveryUpstreams discoveryUpstreams
     */
    public void setDiscoveryUpstreams(final List<DiscoveryUpstream> discoveryUpstreams) {
        this.discoveryUpstreams = discoveryUpstreams;
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

    public static class Discovery {

        /**
         * discovery id.
         */
        private String id;

        /**
         * discovery type.
         */
        private String discoveryType;

        /**
         * serviceList.
         */
        private String serverList;

        private String props;

        /**
         * getId.
         *
         * @return id
         */
        public String getId() {
            return id;
        }

        /**
         * setId.
         *
         * @param id id
         */
        public void setId(final String id) {
            this.id = id;
        }

        /**
         * get type.
         *
         * @return type
         */
        public String getDiscoveryType() {
            return discoveryType;
        }

        /**
         * set type.
         *
         * @param discoveryType type
         */
        public void setDiscoveryType(final String discoveryType) {
            this.discoveryType = discoveryType;
        }

        /**
         * get serviceList.
         *
         * @return serviceList
         */
        public String getServerList() {
            return serverList;
        }

        /**
         * set serverList.
         *
         * @param serverList serverList
         */
        public void setServerList(final String serverList) {
            this.serverList = serverList;
        }

        /**
         * get props.
         *
         * @return props
         */
        public String getProps() {
            return props;
        }

        /**
         * set props.
         *
         * @param props props
         */
        public void setProps(final String props) {
            this.props = props;
        }
    }

    /**
     * the discovery upstream.
     */
    public static class DiscoveryUpstream {

        /**
         * id.
         */
        private String id;

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
        private Integer status;

        /**
         * weight.
         */
        private Integer weight;

        /**
         * props.
         */
        private String props;
        
        /**
         * namespaceId.
         */
        private String namespaceId;

        /**
         * get id.
         *
         * @return id
         */
        public String getId() {
            return id;
        }

        /**
         * set id.
         *
         * @param id id
         */
        public void setId(final String id) {
            this.id = id;
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
        public Integer getStatus() {
            return status;
        }

        /**
         * setStatus.
         *
         * @param status status
         */
        public void setStatus(final Integer status) {

            this.status = status;
        }

        /**
         * getWeight.
         *
         * @return weight
         */
        public Integer getWeight() {
            return weight;
        }

        /**
         * setWeight.
         *
         * @param weight weight
         */
        public void setWeight(final Integer weight) {
            this.weight = weight;
        }

        /**
         * get props.
         *
         * @return props
         */
        public String getProps() {
            return props;
        }

        /**
         * set props.
         *
         * @param props props
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
         * @param namespaceId namespaceId
         */
        public void setNamespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
        }
    }

}
