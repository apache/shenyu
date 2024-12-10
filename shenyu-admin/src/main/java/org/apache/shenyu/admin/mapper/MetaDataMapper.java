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

package org.apache.shenyu.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * The interface Meta data mapper.
 */
@Mapper
public interface MetaDataMapper extends ExistProvider {
    
    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);
    
    /**
     * Select meta data do by id and namespaceId.
     *
     * @param id the id
     * @return the meta data do
     */
    MetaDataDO selectById(String id);
    
    /**
     * Select a list of MetaDataDOs by idList and namespaceId.
     *
     * @param idList a list of ids
     * @param namespaceId namespaceId
     * @return a list of MetaDataDOs
     */
    List<MetaDataDO> selectByIdListAndNamespaceId(@Param("idList") List<String> idList, @Param("namespaceId") String namespaceId);
    
    /**
     * Find all list.
     *
     * @return the list
     */
    List<MetaDataDO> findAll();

    /**
     * Find all list.
     *
     * @param namespaceId the namespaceId
     * @return the list
     */
    List<MetaDataDO> findAllByNamespaceId(String namespaceId);

    /**
     * Find all list.
     *
     * @param namespaceIds the namespaceIds
     * @return the list
     */
    List<MetaDataDO> findAllByNamespaceIds(List<String> namespaceIds);
    
    /**
     * Find meta data do by path and namespaceId.
     *
     * @param path the path
     * @param namespaceId namespaceId
     * @return the meta data do
     */
    MetaDataDO findByPathAndNamespaceId(String path, String namespaceId);
    
    /**
     * Find meta data do by service name and method and namespaceId.
     *
     * @param serviceName the service name
     * @param methodName  the methodName
     * @param namespaceId namespaceId
     * @return the meta data do
     */
    List<MetaDataDO> findByServiceNameAndMethodAndNamespaceId(@Param("serviceName") String serviceName, @Param("methodName") String methodName, @Param("namespaceId") String namespaceId);
    
    /**
     * Select by query list.
     *
     * @param metaDataQuery the meta data query
     * @return the list
     */
    List<MetaDataDO> selectByQuery(MetaDataQuery metaDataQuery);
    
    /**
     * Select all list.
     *
     * @return the list
     */
    List<MetaDataDO> selectAll();
    
    /**
     * Count by query integer.
     *
     * @param metaDataQuery the meta data query
     * @return the integer
     */
    Integer countByQuery(MetaDataQuery metaDataQuery);
    
    /**
     * Insert int.
     *
     * @param metaDataDO the meta data do
     * @return the int
     */
    int insert(MetaDataDO metaDataDO);
    
    /**
     * Update int.
     *
     * @param metaDataDO the meta data do
     * @return the int
     */
    int update(MetaDataDO metaDataDO);
    
    /**
     * Update enable int.
     *
     * @param metaDataDO the meta data do
     * @return the int
     */
    int updateEnable(MetaDataDO metaDataDO);
    
    /**
     * update enable batch.
     *
     * @param idList  the ids
     * @param enabled the status
     * @return the count
     */
    int updateEnableBatch(@Param("idList") List<String> idList, @Param("enabled") Boolean enabled);
    
    /**
     * Delete int.
     *
     * @param id the id
     * @return the int
     */
    int delete(String id);
    
    /**
     * batch delete by a list of ids and namespaceId.
     *
     * @param idList a list of ids
     * @param namespaceId namespaceId
     * @return the count of deleted
     */
    int deleteByIdListAndNamespaceId(@Param("idList") List<String> idList, @Param("namespaceId") String namespaceId);
    
    /**
     * the path is existed.
     *
     * @param path path
     * @param namespaceId namespaceId
     * @return existed
     */
    Boolean pathExisted(@Param("path") Serializable path, @Param("namespaceId") String namespaceId);
    
    /**
     * the path is existed.
     *
     * @param path    path
     * @param exclude exclude
     * @return existed
     */
    Boolean pathExistedExclude(@Param("path") Serializable path, @Param("exclude") List<String> exclude);
}
