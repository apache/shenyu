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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;

/**
 * DiscoveryTransfer.
 */
public enum DiscoveryTransfer {
    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * mapToDo.
     *
     * @param discoveryUpstreamData discoveryUpstreamData
     * @return DiscoveryUpstreamDO
     */
    public DiscoveryUpstreamDO mapToDo(DiscoveryUpstreamData discoveryUpstreamData) {
        return DiscoveryUpstreamDO.builder()
                .discoveryHandlerId(discoveryUpstreamData.getDiscoveryHandlerId())
                .id(discoveryUpstreamData.getId())
                .protocol(discoveryUpstreamData.getProtocol())
                .status(discoveryUpstreamData.getStatus())
                .weight(discoveryUpstreamData.getWeight())
                .props(discoveryUpstreamData.getProps())
                .url(discoveryUpstreamData.getUrl())
                .dateUpdated(discoveryUpstreamData.getDateUpdated())
                .dateCreated(discoveryUpstreamData.getDateCreated()).build();
    }

    /**
     * mapToData.
     *
     * @param discoveryUpstreamDO discoveryUpstreamDO
     * @return DiscoveryUpstreamData
     */
    public DiscoveryUpstreamData mapToData(DiscoveryUpstreamDO discoveryUpstreamDO) {
        DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
        discoveryUpstreamData.setId(discoveryUpstreamDO.getId());
        discoveryUpstreamData.setProtocol(discoveryUpstreamDO.getProtocol());
        discoveryUpstreamData.setUrl(discoveryUpstreamDO.getUrl());
        discoveryUpstreamData.setStatus(discoveryUpstreamDO.getStatus());
        discoveryUpstreamData.setDiscoveryHandlerId(discoveryUpstreamDO.getDiscoveryHandlerId());
        discoveryUpstreamData.setWeight(discoveryUpstreamDO.getWeight());
        discoveryUpstreamData.setProps(discoveryUpstreamDO.getProps());
        discoveryUpstreamData.setDateUpdated(discoveryUpstreamDO.getDateUpdated());
        discoveryUpstreamData.setDateCreated(discoveryUpstreamDO.getDateCreated());
        return discoveryUpstreamData;
    }

}
