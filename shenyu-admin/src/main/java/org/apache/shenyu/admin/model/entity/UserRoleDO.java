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

import org.apache.shenyu.admin.model.dto.UserRoleDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import reactor.util.StringUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * The User Role Entity.
 */
public final class UserRoleDO extends BaseDO {

    private static final long serialVersionUID = -6072114067735588550L;

    /**
     * user key.
     */
    private String userId;

    /**
     * role key.
     */
    private String roleId;

    public UserRoleDO() {
    }

    public UserRoleDO(final String userId, final String roleId) {
        this.userId = userId;
        this.roleId = roleId;
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
     * builder method.
     *
     * @return builder object.
     */
    public static UserRoleDO.UserRoleDOBuilder builder() {
        return new UserRoleDO.UserRoleDOBuilder();
    }

    /**
     * build UserRoleDO.
     *
     * @param userRoleDTO {@linkplain UserRoleDTO}
     * @return {@linkplain UserRoleDO}
     */
    public static UserRoleDO buildUserRoleDO(final UserRoleDTO userRoleDTO) {
        return Optional.ofNullable(userRoleDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            UserRoleDO userRoleDO = UserRoleDO.builder()
                    .roleId(item.getRoleId())
                    .userId(item.getUserId())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                userRoleDO.setId(UUIDUtils.getInstance().generateShortUuid());
                userRoleDO.setDateCreated(currentTime);
            } else {
                userRoleDO.setId(item.getId());
            }
            return userRoleDO;
        }).orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        UserRoleDO that = (UserRoleDO) o;
        return Objects.equals(userId, that.userId) && Objects.equals(roleId, that.roleId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), userId, roleId);
    }

    public static final class UserRoleDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String userId;

        private String roleId;

        private UserRoleDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return UserRoleDOBuilder.
         */
        public UserRoleDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated
         * @return UserRoleDOBuilder.
         */
        public UserRoleDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return UserRoleDOBuilder.
         */
        public UserRoleDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * userId.
         *
         * @param userId the userId.
         * @return UserRoleDOBuilder.
         */
        public UserRoleDOBuilder userId(final String userId) {
            this.userId = userId;
            return this;
        }

        /**
         * roleId.
         *
         * @param roleId the roleId.
         * @return UserRoleDOBuilder.
         */
        public UserRoleDOBuilder roleId(final String roleId) {
            this.roleId = roleId;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public UserRoleDO build() {
            UserRoleDO userRoleDO = new UserRoleDO();
            userRoleDO.setId(id);
            userRoleDO.setDateCreated(dateCreated);
            userRoleDO.setDateUpdated(dateUpdated);
            userRoleDO.setUserId(userId);
            userRoleDO.setRoleId(roleId);
            return userRoleDO;
        }
    }
}
