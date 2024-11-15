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
import org.apache.shenyu.admin.model.entity.NamespaceDO;
import org.apache.shenyu.admin.model.query.NamespaceQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * DiscoveryMapper.
 */
@Mapper
public interface NamespaceMapper extends ExistProvider {

    /**
     * existed.
     *
     * @param namespaceId id
     * @return existed
     */
    @Override
    Boolean existed(@Param("namespaceId") Serializable namespaceId);

    /**
     * select namespace by id.
     *
     * @param id primary key.
     * @return {@linkplain NamespaceDO}
     */
    NamespaceDO selectById(String id);

    /**
     * select namespace by namespace_id.
     *
     * @param namespaceId primary key.
     * @return {@linkplain NamespaceDO}
     */
    NamespaceDO selectByNamespaceId(String namespaceId);

    /**
     * selectByNamespaceIdsAndName.
     *
     * @param namespaceIds namespaceIds.
     * @param name name.
     * @return namespaceDOList
     */
    List<NamespaceDO> selectByNamespaceIdsAndName(List<String> namespaceIds, String name);

    /**
     * selectAllByNamespaceId.
     *
     * @param name name.
     * @return namespaceDOList
     */
    List<NamespaceDO> selectAllByName(String name);

    /**
     * selectAll.
     *
     * @return namespaceDOList
     */
    List<NamespaceDO> selectAll();

    /**
     * insert namespace.
     *
     * @param namespaceDO namespaceDO.
     * @return rows
     */
    int insert(NamespaceDO namespaceDO);


    /**
     * insert namespace.
     *
     * @param namespaceDO namespaceDO.
     * @return rows
     */
    int insertSelective(NamespaceDO namespaceDO);


    /**
     * update namespace.
     *
     * @param namespaceDO {@linkplain NamespaceDO}
     * @return rows
     */
    int update(NamespaceDO namespaceDO);

    /**
     * update namespaceDO.
     *
     * @param namespaceDO {@linkplain NamespaceDO}
     * @return rows
     */
    int updateSelective(NamespaceDO namespaceDO);


    /**
     * delete namespace by namespace_id.
     *
     * @param namespaceId namespace id.
     * @return rows.
     */
    int deleteByNamespaceId(String namespaceId);


    /**
     * count namespace by query.
     *
     * @param namespaceQuery {@linkplain NamespaceQuery}
     * @return {@linkplain Integer}
     */
    Integer countByQuery(NamespaceQuery namespaceQuery);

    /**
     * select namespace by query.
     *
     * @param namespaceQuery {@linkplain NamespaceQuery}
     * @return {@linkplain List}
     */
    List<NamespaceDO> selectByQuery(NamespaceQuery namespaceQuery);

    /**
     * select namespace by NamespaceId.
     *
     * @param namespaceId namespaceId.
     * @return {@linkplain NamespaceDO}
     */
    List<NamespaceDO> selectByNamespaceIds(List<String> namespaceId);

    /**
     * select namespace by ids.
     *
     * @param ids ids.
     * @return {@linkplain NamespaceDO}
     */
    List<NamespaceDO> selectByIds(List<String> ids);


    /**
     * delete namespace.
     *
     * @param ids primary keys.
     * @return rows int
     */
    int deleteByIds(List<String> ids);
}
