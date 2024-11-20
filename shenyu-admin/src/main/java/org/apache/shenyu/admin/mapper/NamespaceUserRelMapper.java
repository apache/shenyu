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
import org.apache.shenyu.admin.model.entity.NamespaceUserRelDO;

import java.util.List;

/**
 * NamespaceUserRelMapper.
 */
@Mapper
public interface NamespaceUserRelMapper {
    
    /**
     * select by namespaceId and userId.
     *
     * @param namespaceId namespaceId
     * @param userId userId
     * @return {@linkplain NamespaceUserRelDO}
     */
    NamespaceUserRelDO selectByNamespaceIdAndUserId(String namespaceId, String userId);

    /**
     * select list by userId.
     *
     * @param userId userId
     * @return {@linkplain List}
     */
    List<NamespaceUserRelDO> selectListByUserId(String userId);

    /**
     * select list by namespaceId.
     *
     * @param namespaceId namespaceId
     * @return {@linkplain List}
     */
    List<NamespaceUserRelDO> selectListByNamespaceId(String namespaceId);
    
    /**
     * insert.
     *
     * @param namespaceUserRelDO {@linkplain NamespaceUserRelDO}
     */
    void insertSelective(NamespaceUserRelDO namespaceUserRelDO);
}
