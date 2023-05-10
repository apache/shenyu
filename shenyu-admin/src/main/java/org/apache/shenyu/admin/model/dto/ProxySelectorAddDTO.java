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

package org.apache.shenyu.admin.model.dto;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * ProxySelectorAddDTO.
 */
public class ProxySelectorAddDTO implements Serializable {

    private static final long serialVersionUID = 1970651564316607656L;

    /**
     * proxy name.
     */
    @NotBlank
    private String name;

    /**
     * plugin name.
     */
    @NotBlank
    private String pluginName;

    /**
     * proxy forward port.
     */
    @NotNull
    private Integer forwardPort;

    @NotBlank
    private String type;

    /**
     * other field.
     */
    @NotBlank
    private String props;

    /**
     * discovery.
     */
    @NotNull
    private Discovery discovery;

    /**
     * discovery upstream list.
     */
    @NotNull
    private List<DiscoveryUpstream> discoveryUpstreams;

    /**
     * get name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get plugin name.
     *
     * @return plugin name
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * set plugin name.
     *
     * @param pluginName plugin name
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * get forward port.
     *
     * @return forward port
     */
    public Integer getForwardPort() {
        return forwardPort;
    }

    /**
     * set forward port.
     * @param forwardPort forward port
     */
    public void setForwardPort(final Integer forwardPort) {
        this.forwardPort = forwardPort;
    }

    /**
     * get type.
     *
     * @return type
     */
    public String getType() {
        return type;
    }

    /**
     * set type.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
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
     * get discovery.
     *
     * @return discovery
     */
    public Discovery getDiscovery() {
        return discovery;
    }

    /**
     * set discovery.
     *
     * @param discovery discovery
     */
    public void setDiscovery(final Discovery discovery) {
        this.discovery = discovery;
    }

    /**
     * get discovery upstream list.
     *
     * @return discovery upstream list
     */
    public List<DiscoveryUpstream> getDiscoveryUpstreams() {
        return discoveryUpstreams;
    }

    /**
     * set discovery upstream list.
     *
     * @param discoveryUpstreams discovery upstream list
     */
    public void setDiscoveryUpstreams(final List<DiscoveryUpstream> discoveryUpstreams) {
        this.discoveryUpstreams = discoveryUpstreams;
    }

    /**
     * get discovery.
     */
    public static class Discovery {

        /**
         * discovery type.
         */
        @NotNull(message = "type not null")
        private String discoveryType;

        /**
         * handler.
         */
        @NotNull(message = "handler not null")
        private String handler;

        /**
         * serviceList.
         */
        @NotNull(message = "serviceList not null")
        private String serviceList;

        /**
         * listenerNode.
         */
        @NotNull(message = "listenerNode not null")
        private String listenerNode;

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
         * get handler.
         *
         * @return handler
         */
        public String getHandler() {
            return handler;
        }

        /**
         * set handler.
         *
         * @param handler handler
         */
        public void setHandler(final String handler) {
            this.handler = handler;
        }

        /**
         * get serviceList.
         *
         * @return serviceList
         */
        public String getServiceList() {
            return serviceList;
        }

        /**
         * set serviceList.
         *
         * @param serviceList serviceList
         */
        public void setServiceList(final String serviceList) {
            this.serviceList = serviceList;
        }

        /**
         * get listenerNode.
         *
         * @return listenerNode
         */
        public String getListenerNode() {
            return listenerNode;
        }

        /**
         * set listenerNode.
         *
         * @param listenerNode listenerNode
         */
        public void setListenerNode(final String listenerNode) {
            this.listenerNode = listenerNode;
        }
    }

    /**
     * the discovery upstream.
     */
    public static class DiscoveryUpstream {

        /**
         * protocol.
         */
        @NotBlank(message = "protocol不能为空")
        private String protocol;

        /**
         * url.
         */
        @NotBlank(message = "url不能为空")
        private String url;

        /**
         * status.
         */
        @NotNull(message = "status不能为空")
        private Integer status;

        /**
         * weight.
         */
        @NotNull(message = "weight不能为空")
        private Integer weight;

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
    }
}
