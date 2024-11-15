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
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * DiscoveryMapper.
 */
@Mapper
public interface DiscoveryMapper extends ExistProvider {

    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);

    /**
     * select discovery by id.
     *
     * @param id primary key.
     * @return {@linkplain DiscoveryDO}
     */
    DiscoveryDO selectById(String id);

    /**
     * selectAll.
     *
     * @return discoveryDOList
     */
    List<DiscoveryDO> selectAll();

    /**
     * selectAllByNamespaceId.
     *
     * @param namespaceId the namespaceId
     * @return discoveryDOList
     */
    List<DiscoveryDO> selectAllByNamespaceId(String namespaceId);

    /**
     * selectAllByNamespaceIds.
     *
     * @param namespaceIds the namespaceIds
     * @return discoveryDOList
     */
    List<DiscoveryDO> selectAllByNamespaceIds(List<String> namespaceIds);

    /**
     * select discovery by plugin name and level.
     *
     * @param pluginName plugin name
     * @param level      level
     * @param namespaceId namespaceId
     * @return {@linkplain DiscoveryDO}
     */
    DiscoveryDO selectByPluginNameAndLevelAndNamespaceId(@Param("pluginName") String pluginName, @Param("level") String level, @Param("namespaceId") String namespaceId);

    /**
     * insert discovery.
     *
     * @param discovery discovery.
     * @return rows
     */
    int insert(DiscoveryDO discovery);

    /**
     * insert discovery.
     *
     * @param discoveryDO discovery.
     * @return rows
     */
    int insertSelective(DiscoveryDO discoveryDO);

    /**
     * update discovery.
     *
     * @param discoveryDO {@linkplain DiscoveryDO}
     * @return rows
     */
    int update(DiscoveryDO discoveryDO);

    /**
     * update discovery.
     *
     * @param discoveryDO {@linkplain DiscoveryDO}
     * @return rows
     */
    int updateSelective(DiscoveryDO discoveryDO);

    /**
     * delete discovery by id.
     *
     * @param id primary key.
     * @return rows.
     */
    int delete(String id);

    /**
     * selectBySelectorNameAndPluginName.
     *
     * @param selectorName selectorName
     * @param pluginName   pluginName
     * @return DiscoveryDO
     */
    DiscoveryDO selectBySelectorNameAndPluginName(String selectorName, String pluginName);

}
