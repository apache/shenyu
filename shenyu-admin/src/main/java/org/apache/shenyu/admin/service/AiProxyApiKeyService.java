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

import org.apache.shenyu.admin.model.dto.ProxyApiKeyDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.ProxyApiKeyQuery;
import org.apache.shenyu.admin.model.vo.ProxyApiKeyVO;

import java.util.List;

/**
 * AiProxyApiKeyService.
 */
public interface AiProxyApiKeyService extends PageService<ProxyApiKeyQuery, ProxyApiKeyVO> {

    /**
     * create.
     *
     * @param dto        dto
     * @param selectorId selector id from path
     * @return affected rows
     */
    int create(ProxyApiKeyDTO dto, String selectorId);

    /**
     * update.
     *
     * @param dto dto
     * @return affected rows
     */
    int update(ProxyApiKeyDTO dto);

    /**
     * find by id.
     *
     * @param id id
     * @return vo
     */
    ProxyApiKeyVO findById(String id);

    /**
     * find by ids.
     *
     * @param ids ids
     * @return list of vo
     */
    List<ProxyApiKeyVO> findByIds(List<String> ids);

    /**
     * delete by ids.
     *
     * @param ids ids
     * @return affected rows
     */
    int delete(List<String> ids);

    /**
     * batch enable/disable.
     *
     * @param ids     ids
     * @param enabled enabled
     * @return result message (empty means success)
     */
    String enabled(List<String> ids, Boolean enabled);

    /**
     * list by page.
     *
     * @param query query
     * @return pager
     */
    CommonPager<ProxyApiKeyVO> listByPage(ProxyApiKeyQuery query);

    /**
     * List all proxy api key data across all namespaces.
     *
     * @return list of {@link org.apache.shenyu.common.dto.ProxyApiKeyData}
     */
    List<org.apache.shenyu.common.dto.ProxyApiKeyData> listAll();

    /**
     * Publish REFRESH event contains all proxy api key data (all namespaces).
     */
    void syncData();

    /**
     * Publish REFRESH event contains proxy api key data of specific namespace.
     *
     * @param namespaceId namespace id
     */
    void syncDataByNamespaceId(String namespaceId);
}