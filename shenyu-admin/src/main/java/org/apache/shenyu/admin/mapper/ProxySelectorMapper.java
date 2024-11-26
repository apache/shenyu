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
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.query.ProxySelectorQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

@Mapper
public interface ProxySelectorMapper extends ExistProvider {

    /**
     * id existed.
     *
     * @param key id
     * @return true or false
     */
    @Override
    Boolean existed(@Param("id") Serializable key);

    /**
     * selectByQuery.
     *
     * @param query query
     * @return proxySelectorDO list
     */
    List<ProxySelectorDO> selectByQuery(ProxySelectorQuery query);

    /**
     * nameExisted.
     *
     * @param name name
     * @return true or false
     */
    Boolean nameExisted(@Param("name") Serializable name);

    /**
     * insert.
     *
     * @param proxySelectorDO proxySelectorDO
     * @return rows int
     */
    int insert(ProxySelectorDO proxySelectorDO);

    /**
     * update.
     *
     * @param proxySelectorDO proxySelectorDO
     * @return rows int
     */
    int update(ProxySelectorDO proxySelectorDO);

    /**
     * selectByIdAndNamespaceId.
     *
     * @param id id
     * @return ProxySelectorDO
     */
    ProxySelectorDO selectById(@Param("id") String id);

    /**
     * selectByIdsAndNamespaceId.
     *
     * @param ids id list
     * @return proxySelectorDO list
     */
    List<ProxySelectorDO> selectByIds(@Param("ids") List<String> ids);

    /**
     * deleteByIdsAndNamespaceId.
     *
     * @param ids id list
     * @return rows int
     */
    int deleteByIds(@Param("ids") List<String> ids);


    /**
     * selectByHandlerId.
     *
     * @param discoveryHandlerId discoveryHandlerId.
     * @return ProxySelectorDO
     */
    ProxySelectorDO selectByHandlerId(@Param("discoveryHandlerId") String discoveryHandlerId);


    /**
     * selectByDiscoveryId.
     *
     * @param discoveryId discoveryId
     * @return proxySelectorDOList
     */
    List<ProxySelectorDO> selectByDiscoveryId(@Param("discoveryId") String discoveryId);

    /**
     * selectAllByNamespaceId.
     *
     * @return ProxySelectorDOList
     */
    List<ProxySelectorDO> selectAll();

    /**
     * selectAllByNamespaceId.
     *
     * @param namespaceId namespaceId
     * @return ProxySelectorDOList
     */
    List<ProxySelectorDO> selectByNamespaceId(String namespaceId);

}
