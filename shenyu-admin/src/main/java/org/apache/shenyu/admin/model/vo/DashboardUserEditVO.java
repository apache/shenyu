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

import org.apache.shenyu.admin.transfer.DashboardUserTransfer;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * this is dashboard user for role.
 */
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

    public DashboardUserEditVO() {
    }

    /**
     * Gets the value of roles.
     *
     * @return the value of roles
     */
    public List<RoleVO> getRoles() {
        return roles;
    }

    /**
     * Sets the roles.
     *
     * @param roles roles
     */
    public void setRoles(final List<RoleVO> roles) {
        this.roles = roles;
    }

    /**
     * Gets the value of allRoles.
     *
     * @return the value of allRoles
     */
    public List<RoleVO> getAllRoles() {
        return allRoles;
    }

    /**
     * Sets the allRoles.
     *
     * @param allRoles allRoles
     */
    public void setAllRoles(final List<RoleVO> allRoles) {
        this.allRoles = allRoles;
    }

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
            DashboardUserEditVO vo = DashboardUserTransfer.INSTANCE.transfer2EditVO(dashboardUserVO);
            vo.setRoles(roles);
            vo.setAllRoles(allRoles);
            return vo;
        }).orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof DashboardUserEditVO)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        DashboardUserEditVO that = (DashboardUserEditVO) o;
        return Objects.equals(roles, that.roles) && Objects.equals(allRoles, that.allRoles);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), roles, allRoles);
    }
}
