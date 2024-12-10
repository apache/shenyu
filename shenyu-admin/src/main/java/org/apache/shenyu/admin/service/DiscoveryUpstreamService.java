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
