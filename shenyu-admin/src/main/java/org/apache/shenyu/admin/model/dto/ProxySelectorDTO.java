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
 * proxy selector dto.
 */
public class ProxySelectorDTO implements Serializable {

    private static final long serialVersionUID = 3004856980753743799L;

    /**
     * id.
     */
    @Existed(provider = ProxySelectorMapper.class, nullOfIgnore = true, message = "proxy selector not exited")
    private String id;

    /**
     * proxy name.
     */
    @NotBlank
    private String name;

    /**
     * handler.
     */
    @NotBlank
    private String handler;

    /**
     * proxy type for tcp, upd, ws.
     */
    @NotBlank
    private String type;

    /**
     * proxy forward port.
     */
    @NotNull
    private Integer forwardPort;

    /**
     * other field.
     */
    @NotBlank
    private String props;

    /**
     * discovery.
     */
    @NotNull
    private ProxySelectorAddDTO.Discovery discovery;

    /**
     * discovery upstream list.
     */
    @NotNull
    private List<ProxySelectorAddDTO.DiscoveryUpstream> discoveryUpstreams;

    /**
     * get id.
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
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
     * getForwardPort.
     *
     * @return forwardPort
     */
    public Integer getForwardPort() {
        return forwardPort;
    }

    /**
     * setForwardPort.
     *
     * @param forwardPort forwardPort
     */
    public void setForwardPort(final Integer forwardPort) {
        this.forwardPort = forwardPort;
    }

    /**
     * getProps.
     *
     * @return props
     */
    public String getProps() {
        return props;
    }

    /**
     * setProps.
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
    public ProxySelectorAddDTO.Discovery getDiscovery() {
        return discovery;
    }

    /**
     * set discovery.
     *
     * @param discovery discovery
     */
    public void setDiscovery(final ProxySelectorAddDTO.Discovery discovery) {
        this.discovery = discovery;
    }

    /**
     * get discovery upstream list.
     *
     * @return discovery upstream list
     */
    public List<ProxySelectorAddDTO.DiscoveryUpstream> getDiscoveryUpstreams() {
        return discoveryUpstreams;
    }

    /**
     * set discovery upstream list.
     *
     * @param discoveryUpstreams discovery upstream list
     */
    public void setDiscoveryUpstreams(final List<ProxySelectorAddDTO.DiscoveryUpstream> discoveryUpstreams) {
        this.discoveryUpstreams = discoveryUpstreams;
    }

    /**
     * get discovery.
     */
    public static class Discovery {

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
         * @return props
         */
        public String getProps() {
            return props;
        }

        /**
         * set props.
         * @param props props
         */
        public void setProps(String props) {
            this.props = props;
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
    }
}
