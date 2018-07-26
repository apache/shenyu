/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.dto.DashboardUserDTO;
import org.dromara.soul.admin.entity.DashboardUserDO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.DashboardUserQuery;

/**
 * this is dashboard user service.
 *
 * @author jiangxiaofeng(Nicholas)
 */
public interface DashboardUserService {

    /**
     * save or update dashboard user.
     *
     * @param dashboardUserDTO {@linkplain DashboardUserDTO}
     * @return rows
     */
    int saveOrUpdate(DashboardUserDTO dashboardUserDTO);

    /**
     * enabled or disabled dashboard user.
     *
     * @param dashboardUserDTO {@linkplain DashboardUserDTO}
     * @return rows
     */
    int enabled(DashboardUserDTO dashboardUserDTO);

    /**
     * find dashboard user by id.
     *
     * @param id pk.
     * @return {@linkplain DashboardUserDO}
     */
    DashboardUserDO findById(String id);

    /**
     * find page of dashboard user by query.
     *
     * @param dashboardUserQuery {@linkplain DashboardUserQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<DashboardUserDO> listByPage(DashboardUserQuery dashboardUserQuery);
}
