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

package org.apache.shenyu.admin.model.dto;

import javax.validation.constraints.NotBlank;
import java.util.Objects;

/**
 * this User Role Dto.
 */
public class UserRoleDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * user key.
     */
    @NotBlank
    private String userId;

    /**
     * role key.
     */
    @NotBlank
    private String roleId;

    public UserRoleDTO() {
    }

    public UserRoleDTO(final String id, final String userId, final String roleId) {
        this.id = id;
        this.userId = userId;
        this.roleId = roleId;
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
     * builder method.
     *
     * @return builder object.
     */
    public static UserRoleDTO.UserRoleDTOBuilder builder() {
        return new UserRoleDTO.UserRoleDTOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof UserRoleDTO)) {
            return false;
        }
        UserRoleDTO that = (UserRoleDTO) o;
        return Objects.equals(id, that.id) && Objects.equals(userId, that.userId) && Objects.equals(roleId, that.roleId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, userId, roleId);
    }

    public static final class UserRoleDTOBuilder {

        private String id;

        private String userId;

        private String roleId;

        private UserRoleDTOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return UserRoleDTOBuilder.
         */
        public UserRoleDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * userId.
         *
         * @param userId the userId.
         * @return UserRoleDTOBuilder.
         */
        public UserRoleDTOBuilder userId(final String userId) {
            this.userId = userId;
            return this;
        }

        /**
         * roleId.
         *
         * @param roleId the roleId.
         * @return UserRoleDTOBuilder.
         */
        public UserRoleDTOBuilder roleId(final String roleId) {
            this.roleId = roleId;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public UserRoleDTO build() {
            UserRoleDTO userRoleDTO = new UserRoleDTO();
            userRoleDTO.setId(id);
            userRoleDTO.setUserId(userId);
            userRoleDTO.setRoleId(roleId);
            return userRoleDTO;
        }
    }
}
