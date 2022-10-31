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

import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.admin.model.dto.DashboardUserModifyPasswordDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.DashboardUserQuery;
import org.apache.shenyu.admin.model.vo.DashboardUserEditVO;
import org.apache.shenyu.admin.model.vo.DashboardUserVO;
import org.apache.shenyu.admin.model.vo.LoginDashboardUserVO;

import java.util.Set;

/**
 * this is dashboard user service.
 */
public interface DashboardUserService {

    /**
     * create or update dashboard user.
     *
     * @param dashboardUserDTO {@linkplain DashboardUserDTO}
     * @return rows
     */
    int createOrUpdate(DashboardUserDTO dashboardUserDTO);
    
    /**
     * create dashboard user.
     *
     * @param dashboardUserDTO {@linkplain DashboardUserDTO}
     * @return rows
     */
    int create(DashboardUserDTO dashboardUserDTO);
    
    /**
     *  update dashboard user.
     *
     * @param dashboardUserDTO {@linkplain DashboardUserDTO}
     * @return rows
     */
    int update(DashboardUserDTO dashboardUserDTO);

    /**
     * delete dashboard users.
     *
     * @param ids primary key.
     * @return rows
     */
    int delete(Set<String> ids);

    /**
     * find dashboard user by id.
     *
     * @param id primary key.
     * @return {@linkplain DashboardUserVO}
     */
    DashboardUserEditVO findById(String id);

    /**
     * find dashboard user by username.
     *
     * @param username username.
     * @return {@linkplain DashboardUserVO}
     */
    DashboardUserVO findByUserName(String username);

    /**
     * find dashboard user by query.
     *
     * @param userName user name
     * @param password user password
     * @return {@linkplain DashboardUserVO}
     */
    DashboardUserVO findByQuery(String userName, String password);

    /**
     * find page of dashboard user by query.
     *
     * @param dashboardUserQuery {@linkplain DashboardUserQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<DashboardUserVO> listByPage(DashboardUserQuery dashboardUserQuery);

    /**
     * To deal with the admin login.
     *
     * @param userName default username is admin
     * @param password admin password
     * @return {@linkplain LoginDashboardUserVO}
     */
    LoginDashboardUserVO login(String userName, String password);

    /**
     * modify password.
     *
     * @param dashboardUserModifyPasswordDTO {@linkplain DashboardUserModifyPasswordDTO}
     * @return rows
     */
    int modifyPassword(DashboardUserModifyPasswordDTO dashboardUserModifyPasswordDTO);
}
