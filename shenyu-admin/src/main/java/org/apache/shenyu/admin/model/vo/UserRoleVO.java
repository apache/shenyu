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

import org.apache.shenyu.admin.model.entity.UserRoleDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.util.Optional;

/**
 * this is user role for web front..
 */
public class UserRoleVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * user key.
     */
    private String userId;

    /**
     * role key.
     */
    private String roleId;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    public UserRoleVO() {
    }

    public UserRoleVO(final String id, final String userId, final String roleId, final String dateCreated, final String dateUpdated) {
        this.id = id;
        this.userId = userId;
        this.roleId = roleId;
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
     * Gets the value of userId.
     *
     * @return the value of userId
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Sets the userId.
     *
     * @param userId userId
     */
    public void setUserId(final String userId) {
        this.userId = userId;
    }

    /**
     * Gets the value of roleId.
     *
     * @return the value of roleId
     */
    public String getRoleId() {
        return roleId;
    }

    /**
     * Sets the roleId.
     *
     * @param roleId roleId
     */
    public void setRoleId(final String roleId) {
        this.roleId = roleId;
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
     * build roleDO.
     *
     * @param userRoleDO {@linkplain UserRoleDO}
     * @return {@linkplain UserRoleVO}
     */
    public static UserRoleVO buildUserRoleVO(final UserRoleDO userRoleDO) {
        return Optional.ofNullable(userRoleDO).map(item -> new UserRoleVO(item.getId(), item.getUserId(), item.getRoleId(),
                DateUtils.localDateTimeToString(item.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(item.getDateUpdated().toLocalDateTime()))).orElse(null);
    }
}
