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

package org.apache.shenyu.admin.model.entity;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * DashboardUserDO.
 */
public final class DashboardUserDO extends BaseDO {

    private static final long serialVersionUID = 3464935043890680423L;

    /**
     * user name.
     */
    private String userName;

    /**
     * user password.
     */
    private String password;

    /**
     * dashboard role.
     */
    private Integer role;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * current role list.
     */
    private List<String> roles;

    public DashboardUserDO() {
    }

    public DashboardUserDO(final String userName, final String password, final Integer role, final Boolean enabled, final List<String> roles) {
        this.userName = userName;
        this.password = password;
        this.role = role;
        this.enabled = enabled;
        this.roles = roles;
    }

    /**
     * Gets the value of userName.
     *
     * @return the value of userName
     */
    public String getUserName() {
        return userName;
    }

    /**
     * Sets the userName.
     *
     * @param userName userName
     */
    public void setUserName(final String userName) {
        this.userName = userName;
    }

    /**
     * Gets the value of password.
     *
     * @return the value of password
     */
    public String getPassword() {
        return password;
    }

    /**
     * Sets the password.
     *
     * @param password password
     */
    public void setPassword(final String password) {
        this.password = password;
    }

    /**
     * Gets the value of role.
     *
     * @return the value of role
     */
    public Integer getRole() {
        return role;
    }

    /**
     * Sets the role.
     *
     * @param role role
     */
    public void setRole(final Integer role) {
        this.role = role;
    }

    /**
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Gets the value of roles.
     *
     * @return the value of roles
     */
    public List<String> getRoles() {
        return roles;
    }

    /**
     * Sets the roles.
     *
     * @param roles roles
     */
    public void setRoles(final List<String> roles) {
        this.roles = roles;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static DashboardUserDO.DashboardUserDOBuilder builder() {
        return new DashboardUserDO.DashboardUserDOBuilder();
    }

    /**
     * build dashboardUserDO.
     *
     * @param dashboardUserDTO {@linkplain DashboardUserDTO}
     * @return {@linkplain DashboardUserDO}
     */
    public static DashboardUserDO buildDashboardUserDO(final DashboardUserDTO dashboardUserDTO) {
        return Optional.ofNullable(dashboardUserDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            DashboardUserDO dashboardUserDO = DashboardUserDO.builder()
                    .userName(item.getUserName())
                    .password(item.getPassword())
                    .role(item.getRole())
                    .roles(item.getRoles())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                dashboardUserDO.setId(UUIDUtils.getInstance().generateShortUuid());
                dashboardUserDO.setEnabled(true);
                dashboardUserDO.setDateCreated(currentTime);
            } else {
                dashboardUserDO.setId(item.getId());
                dashboardUserDO.setEnabled(item.getEnabled());
            }
            return dashboardUserDO;
        }).orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof DashboardUserDO)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        DashboardUserDO that = (DashboardUserDO) o;
        return Objects.equals(userName, that.userName)
                && Objects.equals(password, that.password)
                && Objects.equals(role, that.role)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(roles, that.roles);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), userName, password, role, enabled, roles);
    }

    public static final class DashboardUserDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String userName;

        private String password;

        private Integer role;

        private Boolean enabled;

        private List<String> roles;

        private DashboardUserDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return DashboardUserDOBuilder.
         */
        public DashboardUserDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return DashboardUserDOBuilder.
         */
        public DashboardUserDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return DashboardUserDOBuilder.
         */
        public DashboardUserDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * userName.
         *
         * @param userName the userName.
         * @return DashboardUserDOBuilder.
         */
        public DashboardUserDOBuilder userName(final String userName) {
            this.userName = userName;
            return this;
        }

        /**
         * password.
         *
         * @param password the password.
         * @return DashboardUserDOBuilder.
         */
        public DashboardUserDOBuilder password(final String password) {
            this.password = password;
            return this;
        }

        /**
         * role.
         *
         * @param role the role.
         * @return DashboardUserDOBuilder.
         */
        public DashboardUserDOBuilder role(final Integer role) {
            this.role = role;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return DashboardUserDOBuilder.
         */
        public DashboardUserDOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * roles.
         *
         * @param roles the roles.
         * @return DashboardUserDOBuilder.
         */
        public DashboardUserDOBuilder roles(final List<String> roles) {
            this.roles = roles;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public DashboardUserDO build() {
            DashboardUserDO dashboardUserDO = new DashboardUserDO();
            dashboardUserDO.setId(id);
            dashboardUserDO.setDateCreated(dateCreated);
            dashboardUserDO.setDateUpdated(dateUpdated);
            dashboardUserDO.setUserName(userName);
            dashboardUserDO.setPassword(password);
            dashboardUserDO.setRole(role);
            dashboardUserDO.setEnabled(enabled);
            dashboardUserDO.setRoles(roles);
            return dashboardUserDO;
        }
    }
}
