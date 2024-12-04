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
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;

import java.util.List;

/**
 * DiscoveryRelMapper.
 */
@Mapper
public interface DiscoveryRelMapper {

    /**
     * select discoveryRel by id.
     *
     * @param id primary key.
     * @return {@linkplain DiscoveryRelDO}
     */
    DiscoveryRelDO selectById(String id);

    /**
     * select all discoveryRel.
     *
     * @return {@linkplain DiscoveryRelDO}
     */
    List<DiscoveryRelDO> selectAll();

    /**
     * select discoveryRel by proxy selector id.
     *
     * @param proxySelectorId proxy selector id.
     * @return {@linkplain DiscoveryRelDO}
     */
    DiscoveryRelDO selectByProxySelectorId(String proxySelectorId);

    /**
     * insert discoveryRelDO.
     *
     * @param discoveryRelDO discoveryRelDO.
     * @return rows
     */
    int insert(DiscoveryRelDO discoveryRelDO);

    /**
     * insert discoveryRelDO.
     *
     * @param discoveryRelDO discoveryRelDO.
     * @return rows
     */
    int insertSelective(DiscoveryRelDO discoveryRelDO);

    /**
     * update discoveryRelDO.
     *
     * @param discoveryRelDO {@linkplain DiscoveryRelDO}
     * @return rows
     */
    int update(DiscoveryRelDO discoveryRelDO);

    /**
     * update discoveryRelDO.
     *
     * @param discoveryRelDO {@linkplain DiscoveryRelDO}
     * @return rows
     */
    int updateSelective(DiscoveryRelDO discoveryRelDO);

    /**
     * delete discoveryRel by id.
     *
     * @param id primary key.
     * @return rows.
     */
    int delete(String id);

    /**
     * selectByDiscoveryHandlerId.
     *
     * @param discoveryHandlerId discoveryHandlerId
     * @return DiscoveryRelDO
     */
    DiscoveryRelDO selectByDiscoveryHandlerId(@Param("discoveryHandlerId") String discoveryHandlerId);

    /**
     * deleteByDiscoveryHandlerId.
     *
     * @param discoveryHandlerId discoveryHandlerId
     * @return rows
     */
    int deleteByDiscoveryHandlerId(@Param("discoveryHandlerId") String discoveryHandlerId);

}
