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

import org.apache.shenyu.admin.model.dto.ProxySelectorAddDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.ProxySelectorQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.ProxySelectorVO;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.common.dto.ProxySelectorData;

import java.util.List;

public interface ProxySelectorService {

    /**
     * listByPage.
     *
     * @param query query
     * @return page
     */
    CommonPager<ProxySelectorVO> listByPage(ProxySelectorQuery query);

    /**
     * createOrUpdate.
     *
     * @param proxySelectorAddDTO proxySelectorAddDTO
     * @return the string
     */
    String createOrUpdate(ProxySelectorAddDTO proxySelectorAddDTO);

    /**
     * delete.
     *
     * @param ids id list
     * @return the string
     */
    String delete(List<String> ids);

    /**
     * addProxySelector.
     *
     * @param proxySelectorAddDTO {@link ProxySelectorAddDTO}
     * @return the result
     */
    String create(ProxySelectorAddDTO proxySelectorAddDTO);


    /**
     * bind selector and discoveryHandler.
     *
     * @param proxySelectorAddDTO proxySelectorAddDTO
     * @return the result.
     */
    String bindingDiscoveryHandler(ProxySelectorAddDTO proxySelectorAddDTO);

    /**
     * fetchData.
     *
     * @param discoveryHandlerId discoveryHandlerId
     */
    void fetchData(String discoveryHandlerId);

    /**
     * listAll.
     *
     * @return ProxySelectorDTOList
     */
    List<ProxySelectorData> listAll();

    /**
     * listAll.
     *
     * @param namespaceId namespaceId
     * @return ProxySelectorDTOList
     */
    List<ProxySelectorData> listAllByNamespaceId(String namespaceId);

    /**
     * list all data.
     *
     * @return ProxySelectorVOList
     */
    List<ProxySelectorVO> listAllData();

    /**
     * Import proxy selector list.
     *
     * @param proxySelectorList proxy selector data list
     * @return config import result
     */
    ConfigImportResult importData(List<ProxySelectorData> proxySelectorList);

    /**
     * Import proxy selector list.
     *
     * @param namespace namespace
     * @param proxySelectorList proxy selector data list
     * @param context import context
     * @return config import result
     */
    ConfigImportResult importData(String namespace, List<ProxySelectorData> proxySelectorList, ConfigsImportContext context);
}
