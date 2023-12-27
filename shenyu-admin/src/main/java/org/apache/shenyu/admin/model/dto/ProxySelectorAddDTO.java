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

import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

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
     * id.
     */
    @Existed(provider = ProxySelectorMapper.class, nullOfIgnore = true, message = "proxy selector not exited")
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * proxy name.
     */
    @NotBlank
    private String name;


    /**
     * pluginName.
     */
    @NotBlank
    private String pluginName;

    /**
     * proxy forward port.
     */
    private Integer forwardPort;

    @NotBlank
    private String type;

    /**
     * other field.
     */
    private String props;

    /**
     * listenerNode.
     */
    private String listenerNode;

    /**
     * handler.
     */
    private String handler;

    /**
     * discovery.
     */
    @NotNull
    private Discovery discovery;

    /**
     * discovery upstream list.
     */
    private List<DiscoveryUpstream> discoveryUpstreams;

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
     *
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
     * get discovery.
     */
    public static class Discovery {

        /**
         * discovery id.
         */
        private String id;

        /**
         * discovery type.
         */
        @NotNull(message = "discoveryType not null")
        private String discoveryType;

        /**
         * serviceList.
         */
        @NotNull(message = "serverList not null")
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
        @NotBlank(message = "discovery upstream id不能为空")
        private String id;

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
        private Integer weight;

        /**
         * props.
         */
        private String props;

        /**
         * startupTime.
         */
        private String startupTime;

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
         * get startupTime.
         * @return startupTime
         */
        public String getStartupTime() {
            return startupTime;
        }

        /**
         * setStartupTime.
         *
         * @param startupTime setStartupTime
         */
        public void setStartupTime(final String startupTime) {
            this.startupTime = startupTime;
        }
    }
}
