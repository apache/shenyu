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
import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.admin.model.query.RoleQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * The Role Mapper.
 */
@Mapper
public interface RoleMapper extends ExistProvider {
    
    /**
     * role existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);
    
    /**
     * select role by id.
     *
     * @param id primary key.
     * @return {@linkplain RoleDO}
     */
    RoleDO selectById(String id);
    
    /**
     * select role by query.
     *
     * @param roleQuery {@linkplain RoleQuery}
     * @return {@linkplain List}
     */
    List<RoleDO> selectByQuery(RoleQuery roleQuery);
    
    /**
     * Find by Role Name list.
     *
     * @param roleName the role name
     * @return The role
     */
    RoleDO findByRoleName(String roleName);
    
    /**
     * count role by query.
     *
     * @param roleQuery {@linkplain RoleQuery}
     * @return {@linkplain Integer}
     */
    Integer countByQuery(RoleQuery roleQuery);
    
    /**
     * insert role.
     *
     * @param roleDO {@linkplain RoleDO}
     * @return rows int
     */
    int insert(RoleDO roleDO);
    
    /**
     * insert selective role.
     *
     * @param roleDO {@linkplain RoleDO}
     * @return rows int
     */
    int insertSelective(RoleDO roleDO);
    
    /**
     * update role.
     *
     * @param roleDO {@linkplain RoleDO}
     * @return rows int
     */
    int update(RoleDO roleDO);
    
    /**
     * update selective role.
     *
     * @param roleDO {@linkplain RoleDO}
     * @return rows int
     */
    int updateSelective(RoleDO roleDO);
    
    /**
     * delete role.
     *
     * @param ids primary keys
     * @return rows int
     */
    int delete(List<String> ids);
    
    /**
     * list All.
     *
     * @return {@linkplain List}
     */
    List<RoleDO> selectAll();
    
    /**
     * select by ids.
     *
     * @param ids ids.
     * @return list
     */
    List<RoleDO> selectByIds(@Param("ids") List<String> ids);
}
