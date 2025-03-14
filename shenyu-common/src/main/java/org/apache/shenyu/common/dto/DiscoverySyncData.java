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

import java.util.List;

public class DiscoverySyncData {

    /**
     * selectorId.
     */
    private String selectorId;

    /**
     * pluginName.
     */
    private String pluginName;

    /**
     * selectorName.
     */
    private String selectorName;

    /**
     * upstreamDataList.
     */
    private List<DiscoveryUpstreamData> upstreamDataList;

    /**
     * namespaceId.
     */
    private String namespaceId;

    /**
     * discoveryHandlerId.
     *
     */
    private String discoveryHandlerId;

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
     * getUpstreamDataList.
     *
     * @return upstreamDataList
     */
    public List<DiscoveryUpstreamData> getUpstreamDataList() {
        return upstreamDataList;
    }

    /**
     * setUpstreamDataList.
     *
     * @param upstreamDataList upstreamDataList
     */
    public void setUpstreamDataList(final List<DiscoveryUpstreamData> upstreamDataList) {
        this.upstreamDataList = upstreamDataList;
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
     * discoveryHandlerId.
     *
     * @return DiscoveryHandlerId
     */
    public String getDiscoveryHandlerId() {
        return discoveryHandlerId;
    }

    /**
     * set discoveryHandlerId.
     *
     * @param discoveryHandlerId discoveryHandlerId
     */
    public void setDiscoveryHandlerId(final String discoveryHandlerId) {
        this.discoveryHandlerId = discoveryHandlerId;
    }
}
