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
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * DiscoveryHandlerMapper.
 */
@Mapper
public interface DiscoveryHandlerMapper extends ExistProvider {

    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);

    /**
     * select discovery handler by id.
     *
     * @param id primary key.
     * @return {@linkplain DiscoveryHandlerDO}
     */
    DiscoveryHandlerDO selectById(String id);

    /**
     * insert discovery handler.
     *
     * @param discoveryHandlerDO discovery handler.
     * @return rows
     */
    int insert(DiscoveryHandlerDO discoveryHandlerDO);

    /**
     * insert discovery handler.
     *
     * @param discoveryHandlerDO discovery handler.
     * @return rows
     */
    int insertSelective(DiscoveryHandlerDO discoveryHandlerDO);

    /**
     * update discovery handler.
     *
     * @param discoveryHandlerDO {@linkplain DiscoveryHandlerDO}
     * @return rows
     */
    int update(DiscoveryHandlerDO discoveryHandlerDO);

    /**
     * update discovery handler.
     *
     * @param discoveryHandlerDO {@linkplain DiscoveryHandlerDO}
     * @return rows
     */
    int updateSelective(DiscoveryHandlerDO discoveryHandlerDO);

    /**
     * delete discovery handler by id.
     *
     * @param id primary key.
     * @return rows.
     */
    int delete(String id);

    /**
     * selectByProxySelectorId.
     *
     * @param proxySelectorId proxySelectorId
     * @return DiscoveryHandlerDO
     */
    DiscoveryHandlerDO selectByProxySelectorId(@Param("proxySelectorId") String proxySelectorId);

    /**
     * DiscoveryHandlerDOList.
     *
     * @return DiscoveryHandlerDOListx
     */
    List<DiscoveryHandlerDO> selectAll();

    /**
     * selectByDiscoveryId.
     *
     * @param discoveryId discoveryId
     * @return DiscoveryHandlerDO list
     */
    List<DiscoveryHandlerDO> selectByDiscoveryId(@Param("discoveryId")String discoveryId);

    /**
     * selectBySelectorId.
     *
     * @param selectorId selectorId
     * @return DiscoveryHandlerDO
     */
    DiscoveryHandlerDO selectBySelectorId(String selectorId);
}
