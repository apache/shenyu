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

package org.apache.shenyu.admin.model.vo;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import java.util.List;
import java.util.Optional;

/**
 * this is dashboard user for role.
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class DashboardUserEditVO extends DashboardUserVO {

    private static final long serialVersionUID = 7164406413090154990L;

    /**
     * user role list.
     */
    private List<RoleVO> roles;

    /**
     * all role list.
     */
    private List<RoleVO> allRoles;

    /**
     * get edit user info.
     *
     * @param dashboardUserVO {@linkplain DashboardUserVO}
     * @param roles {@linkplain List}
     * @param allRoles {@linkplain List}
     * @return {@linkplain DashboardUserEditVO}
     */
    public static DashboardUserEditVO buildDashboardUserEditVO(final DashboardUserVO dashboardUserVO, final List<RoleVO> roles, final List<RoleVO> allRoles) {
        return Optional.ofNullable(dashboardUserVO).map(item -> {
            DashboardUserEditVO vo = new DashboardUserEditVO();
            BeanUtils.copyProperties(item, vo);
            vo.setRoles(roles);
            vo.setAllRoles(allRoles);
            return vo;
        }).orElse(null);
    }
}
