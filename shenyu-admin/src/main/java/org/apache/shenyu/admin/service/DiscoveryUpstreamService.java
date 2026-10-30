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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.DiscoveryUpstreamVO;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;

import java.util.List;

public interface DiscoveryUpstreamService {

    /**
     * createOrUpdate.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return the string
     */
    String createOrUpdate(DiscoveryUpstreamDTO discoveryUpstreamDTO);

    /**
     * Create or update upstreams in one transaction.
     *
     * @param upstreams upstreams to create or update
     */
    void createOrUpdateBatch(List<DiscoveryUpstreamDTO> upstreams);


    /**
     * updateBatch.
     *
     * @param discoveryHandlerId       discoveryHandlerId
     * @param discoveryUpstreamDTOList discoveryUpstreamDTOList
     * @return effect
     */
    int updateBatch(String discoveryHandlerId, List<DiscoveryUpstreamDTO> discoveryUpstreamDTOList);


    /**
     * nativeCreateOrUpdate.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     */
    void nativeCreateOrUpdate(DiscoveryUpstreamDTO discoveryUpstreamDTO);

    /**
     * delete.
     *
     * @param ids id list
     * @return the string
     */
    String delete(List<String> ids);

    /**
     * listAll.
     *
     * @return DiscoverySyncDataList
     */
    List<DiscoverySyncData> listAll();

    /**
     * List synchronization data in a single namespace.
     *
     * @param namespaceId namespace id
     * @return synchronization data
     */
    List<DiscoverySyncData> listAllByNamespaceId(String namespaceId);

    /**
     * list all data.
     *
     * @return DiscoveryUpstreamVO
     */
    List<DiscoveryUpstreamVO> listAllData();

    /**
     * list all data.
     *
     * @param namespaceId namespaceId
     * @return DiscoveryUpstreamVO
     */
    List<DiscoveryUpstreamVO> listAllDataByNamespaceId(String namespaceId);

    /**
     * refresh and push event.
     *
     * @param selectorId selectorId
     */
    void refreshBySelectorId(String selectorId);

    /**
     * findBySelectorId.
     *
     * @param selectorId selectorId
     * @return DiscoveryUpstreamDataList
     */
    List<DiscoveryUpstreamData> findBySelectorId(String selectorId);

    /**
     * Whether a selector has authoritative discovery state, including an empty instance list.
     *
     * @param selectorId selector id
     * @return whether a discovery handler exists
     */
    boolean hasDiscoveryHandler(String selectorId);


    /**
     * deleteBySelectorIdAndUrl.
     *
     * @param selectorId selectorId
     * @param url        url
     */
    void deleteBySelectorIdAndUrl(String selectorId, String url);


    /**
     * changeStatusBySelectorIdAndUrl.
     *
     * @param selectorId selectorId
     * @param url        url
     * @param enabled    enabled
     */
    void changeStatusBySelectorIdAndUrl(String selectorId, String url, Boolean enabled);

    /**
     * Update the upstream status unless the address is protected by registration fencing.
     * The protection check and update share one transaction and selector lock.
     *
     * @param selectorId selectorId
     * @param url upstream URL
     * @param enabled enabled
     * @return whether the update was allowed
     */
    boolean tryChangeStatusBySelectorIdAndUrl(String selectorId, String url, Boolean enabled);

    /**
     * Merge a draining marker into the props of the upstream matching the selector
     * and url. Idempotent: the marker is written, never derived from status, so a
     * heartbeat-driven status flip cannot cancel it.
     *
     * @param selectorId selectorId
     * @param url        url
     * @param draining   true to mark draining, false to release the marker
     */
    void markDrainingBySelectorIdAndUrl(String selectorId, String url, boolean draining);

    /**
     * Merge a not-ready marker into the props of the upstream matching the selector
     * and url. Idempotent: the marker is written, never derived from status, so a
     * heartbeat-driven status flip cannot clear it.
     *
     * @param selectorId selectorId
     * @param url        url
     * @param notReady   true to mark not ready, false to release the marker
     */
    void markNotReadyBySelectorIdAndUrl(String selectorId, String url, boolean notReady);

    /**
     * Release both admission markers (draining and not-ready) from the upstream
     * matching the selector and url. Idempotent.
     *
     * @param selectorId selectorId
     * @param url        url
     */
    void markReadyBySelectorIdAndUrl(String selectorId, String url);

    /**
     * Import the discoveryUpstream data list.
     *
     * @param discoveryUpstreamList the discoveryUpstream data
     * @return config import result
     */
    ConfigImportResult importData(List<DiscoveryUpstreamDTO> discoveryUpstreamList);

    /**
     * Import the discoveryUpstream data list.
     *
     * @param namespace the namespace
     * @param discoveryUpstreamList the discoveryUpstream data
     * @param context import context
     * @return config import result
     */
    ConfigImportResult importData(String namespace, List<DiscoveryUpstreamDTO> discoveryUpstreamList,
                                  ConfigsImportContext context);
}
