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

import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Objects;
import java.util.Optional;

/**
 * this is dashboard user view to web front.
 */
public class DashboardUserVO implements Serializable {

    private static final long serialVersionUID = -6493573459758664389L;

    /**
     * primary key.
     */
    private String id;

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
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    public DashboardUserVO() {
    }

    public DashboardUserVO(final String id,
                           final String userName,
                           final String password,
                           final Integer role,
                           final Boolean enabled,
                           final String dateCreated,
                           final String dateUpdated) {
        this.id = id;
        this.userName = userName;
        this.password = password;
        this.role = role;
        this.enabled = enabled;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
    }

    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
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
     * Gets the value of dateCreated.
     *
     * @return the value of dateCreated
     */
    public String getDateCreated() {
        return dateCreated;
    }

    /**
     * Sets the dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final String dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * Gets the value of dateUpdated.
     *
     * @return the value of dateUpdated
     */
    public String getDateUpdated() {
        return dateUpdated;
    }

    /**
     * Sets the dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    /**
     * build dashboardUserVO.
     *
     * @param dashboardUserDO {@linkplain DashboardUserDO}
     * @return {@linkplain DashboardUserVO}
     */
    public static DashboardUserVO buildDashboardUserVO(final DashboardUserDO dashboardUserDO) {
        return Optional.ofNullable(dashboardUserDO)
                .map(item -> new DashboardUserVO(item.getId(), item.getUserName(),
                        item.getPassword(), item.getRole(), item.getEnabled(),
                        DateUtils.localDateTimeToString(item.getDateCreated().toLocalDateTime()),
                        DateUtils.localDateTimeToString(item.getDateUpdated().toLocalDateTime())))
                .orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof DashboardUserVO)) {
            return false;
        }
        DashboardUserVO that = (DashboardUserVO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(userName, that.userName)
                && Objects.equals(password, that.password)
                && Objects.equals(role, that.role)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(dateCreated, that.dateCreated)
                && Objects.equals(dateUpdated, that.dateUpdated);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, userName, password, role, enabled, dateCreated, dateUpdated);
    }
}
