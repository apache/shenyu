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

package org.apache.shenyu.common.dto;

import java.sql.Timestamp;

public class DiscoveryUpstreamData {

    /**
     * primary key.
     */
    private String id;

    /**
     * created time.
     */
    private Timestamp dateCreated;

    /**
     * updated time.
     */
    private Timestamp dateUpdated;


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
    private int status;

    /**
     * weight.
     */
    private int weight;

    /**
     * props.
     */
    private String props;


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
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * Gets the value of dateCreated.
     *
     * @return the value of dateCreated
     */
    public Timestamp getDateCreated() {
        return dateCreated;
    }

    /**
     * Sets the dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Timestamp dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * Gets the value of dateUpdated.
     *
     * @return the value of dateUpdated
     */
    public Timestamp getDateUpdated() {
        return dateUpdated;
    }

    /**
     * setDateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Timestamp dateUpdated) {
        this.dateUpdated = dateUpdated;
    }
}
