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
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.query.DashboardUserQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

/**
 * DashboardUserMapper.
 */
@Mapper
public interface DashboardUserMapper extends ExistProvider {
    
    /**
     * exiated.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);
    
    /**
     * select dashboard user by id.
     *
     * @param id primary key.
     * @return {@linkplain DashboardUserDO}
     */
    DashboardUserDO selectById(String id);
    
    /**
     * select dashboard user by user name.
     *
     * @param userName user name
     * @return {@linkplain DashboardUserDO}
     */
    DashboardUserDO selectByUserName(String userName);
    
    /**
     * find dashboard user by query.
     *
     * @param userName user name
     * @param password user password
     * @return {@linkplain DashboardUserDO}
     */
    DashboardUserDO findByQuery(@Param("userName") String userName, @Param("password") String password);
    
    /**
     * select dashboard user by query.
     *
     * @param dashboardUserQuery {@linkplain DashboardUserQuery}
     * @return {@linkplain List}
     */
    List<DashboardUserDO> selectByQuery(DashboardUserQuery dashboardUserQuery);
    
    /**
     * select by ids.
     *
     * @param ids ids.
     * @return users
     */
    List<DashboardUserDO> selectByIds(@Param("ids") Set<String> ids);
    
    /**
     * count dashboard user by query.
     *
     * @param dashboardUserQuery {@linkplain DashboardUserQuery}
     * @return {@linkplain Integer}
     */
    Integer countByQuery(DashboardUserQuery dashboardUserQuery);
    
    /**
     * insert dashboard user.
     *
     * @param dashboardUserDO {@linkplain DashboardUserDO}
     * @return rows
     */
    int insert(DashboardUserDO dashboardUserDO);
    
    /**
     * insert selective dashboard user.
     *
     * @param dashboardUserDO {@linkplain DashboardUserDO}
     * @return rows
     */
    int insertSelective(DashboardUserDO dashboardUserDO);
    
    /**
     * update dashboard user.
     *
     * @param dashboardUserDO {@linkplain DashboardUserDO}
     * @return rows
     */
    int update(DashboardUserDO dashboardUserDO);
    
    /**
     * update selective dashboard user.
     *
     * @param dashboardUserDO {@linkplain DashboardUserDO}
     * @return rows
     */
    int updateSelective(DashboardUserDO dashboardUserDO);
    
    /**
     * delete dashboard user.
     *
     * @param id primary key.
     * @return rows
     */
    int delete(String id);
    
    /**
     * delete the specified dashboard users.
     *
     * @param idList primary key of dashboard_user
     * @return the count of deleted
     */
    int deleteByIdList(@Param("idList") List<String> idList);
}
