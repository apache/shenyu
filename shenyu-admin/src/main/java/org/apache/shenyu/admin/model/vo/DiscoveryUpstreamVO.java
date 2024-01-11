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

package org.apache.shenyu.admin.model.vo;

public class DiscoveryUpstreamVO {

    /**
     * id.
     */
    private String id;

    /**
     * discoveryHandlerId.
     */
    private String discoveryHandlerId;

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
     * dateCreated.
     */
    private String startupTime;

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
     * getDiscoveryHandlerId.
     *
     * @return discoveryHandlerId
     */
    public String getDiscoveryHandlerId() {
        return discoveryHandlerId;
    }

    /**
     * setDiscoveryHandlerId.
     *
     * @param discoveryHandlerId discoveryHandlerId
     */
    public void setDiscoveryHandlerId(final String discoveryHandlerId) {
        this.discoveryHandlerId = discoveryHandlerId;
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
     * getStartupTime.
     *
     * @return startupTime
     */
    public String getStartupTime() {
        return startupTime;
    }

    /**
     * setStartupTime.
     *
     * @param startupTime startupTime
     */
    public void setStartupTime(final String startupTime) {
        this.startupTime = startupTime;
    }
}
