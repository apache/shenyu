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

import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;

import java.util.List;
import java.util.Map;

/**
 * this is application authority service.
 */
public interface MetaDataService {

    /**
     * save or update mate data.
     * {@link org.apache.shenyu.admin.service.register.AbstractShenyuClientRegisterServiceImpl}
     *
     * @param exist       has been exist meta data {@link MetaDataDO}
     * @param metaDataDTO meta data dto {@link MetaDataRegisterDTO}
     */
    void saveOrUpdateMetaData(MetaDataDO exist, MetaDataRegisterDTO metaDataDTO);

    /**
     * Create or update int.
     *
     * @param metaDataDTO the meta data dto
     * @return the String
     */
    String createOrUpdate(MetaDataDTO metaDataDTO);

    /**
     * delete application authorities by ids and namespaceId.
     *
     * @param ids primary key
     * @param namespaceId namespaceId
     * @return rows int
     */
    int deleteByIdsAndNamespaceId(List<String> ids, String namespaceId);

    /**
     * Find meta data vo by id.
     *
     * @param id the id
     * @return the meta data vo
     */
    MetaDataVO findById(String id);

    /**
     * List by page common pager.
     *
     * @param metaDataQuery the meta data query
     * @return the common pager
     */
    CommonPager<MetaDataVO> listByPage(MetaDataQuery metaDataQuery);

    /**
     * Find all list.
     *
     * @return the list
     */
    List<MetaDataVO> findAll();

    /**
     * Find all group map.
     *
     * @return the map
     */
    Map<String, List<MetaDataVO>> findAllGroup();

    /**
     * List all list.
     *
     * @return the list
     */
    List<MetaData> listAll();

    /**
     * List all vo list.
     *
     * @return the vo list
     */
    List<MetaDataVO> listAllData();

    /**
     * List all vo list.
     *
     * @param namespaceId the namespaceId
     * @return the vo list
     */
    List<MetaDataVO> listAllDataByNamespaceId(String namespaceId);

    /**
     * Enabled by ids and namespaceId.
     *
     * @param ids     the ids
     * @param enabled the enabled
     * @param namespaceId namespaceId
     * @return the string
     */
    String enabledByIdsAndNamespaceId(List<String> ids, Boolean enabled, String namespaceId);

    /**
     * Sync data.
     */
    void syncData();

    /**
     * Sync data by namespaceId.
     *
     * @param namespaceId the namespaceId
     */
    void syncDataByNamespaceId(String namespaceId);

    /**
     * find meta data by path and namespaceId.
     *
     * @param path the path of meta data
     * @param namespaceId namespaceId
     * @return {@link MetaDataDO}
     */
    MetaDataDO findByPathAndNamespaceId(String path, String namespaceId);

    /**
     * find meta data by service's name and method's name and namespaceId.
     *
     * @param serviceName the name of service
     * @param methodName  the name of method
     * @param namespaceId namespaceId
     * @return {@link MetaDataDO}
     */
    MetaDataDO findByServiceNameAndMethodNameAndNamespaceId(String serviceName, String methodName, String namespaceId);

    /**
     * insert MetaDataDO.
     *
     * @param metaDataDO meta data object
     * @return the success rows.
     */
    int insert(MetaDataDO metaDataDO);

    /**
     * Import shenyu meta data.
     *
     * @param metaDataList meta data list
     * @return the config import result
     */
    ConfigImportResult importData(List<MetaDataDTO> metaDataList);

    /**
     * Import shenyu meta data.
     *
     * @param namespace     the namespace
     * @param metaDataList meta data list
     * @return the config import result
     */
    ConfigImportResult importData(String namespace, List<MetaDataDTO> metaDataList);
}
