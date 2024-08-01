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

import org.apache.shenyu.admin.model.constant.RegConstant;
import org.apache.shenyu.admin.utils.FailI18nMessage;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * this is dashboard user from by web front.
 */
public class DashboardUserDTO implements Serializable {

    private static final long serialVersionUID = -7005615329360835626L;

    /**
     * primary key.
     */
    private String id;

    /**
     * user name.
     */
    @NotBlank
    private String userName;

    /**
     * user password.
     */
    @Pattern(regexp = RegConstant.PASSWORD_RULE, message = '{' + FailI18nMessage.PASSWORD_MUST + '}')
    private String password;

    /**
     * dashboard role.
     */
    private Integer role;

    /**
     * current role list.
     */
    private List<@NotBlank String> roles;

    /**
     * whether enabled.
     */
    @NotNull
    private Boolean enabled;

    /**
     * clientId.
     */
    private String clientId;

    public DashboardUserDTO() {
    }

    public DashboardUserDTO(final String id, @NotNull final String userName, final String password, final Integer role, final List<String> roles, final Boolean enabled, final String clientId) {
        this.id = id;
        this.userName = userName;
        this.password = password;
        this.role = role;
        this.roles = roles;
        this.enabled = enabled;
        this.clientId = clientId;
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
     * Gets the value of clientId.
     *
     * @return the value of clientId
     */
    public String getClientId() {
        return clientId;
    }

    /**
     * Sets the clientId.
     *
     * @param clientId clientId
     */
    public void setClientId(final String clientId) {
        this.clientId = clientId;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static DashboardUserDTO.DashboardUserDTOBuilder builder() {
        return new DashboardUserDTO.DashboardUserDTOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof DashboardUserDTO)) {
            return false;
        }
        DashboardUserDTO that = (DashboardUserDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(userName, that.userName)
                && Objects.equals(password, that.password)
                && Objects.equals(role, that.role)
                && Objects.equals(roles, that.roles)
                && Objects.equals(enabled, that.enabled);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, userName, password, role, roles, enabled);
    }

    public static final class DashboardUserDTOBuilder {

        private String id;

        private String userName;

        private String password;

        private Integer role;

        private List<String> roles;

        private Boolean enabled;

        private String clientId;

        private DashboardUserDTOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return DashboardUserDTOBuilder.
         */
        public DashboardUserDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * userName.
         *
         * @param userName the userName.
         * @return DashboardUserDTOBuilder.
         */
        public DashboardUserDTOBuilder userName(final String userName) {
            this.userName = userName;
            return this;
        }

        /**
         * password.
         *
         * @param password the password.
         * @return DashboardUserDTOBuilder.
         */
        public DashboardUserDTOBuilder password(final String password) {
            this.password = password;
            return this;
        }

        /**
         * role.
         *
         * @param role the role.
         * @return DashboardUserDTOBuilder.
         */
        public DashboardUserDTOBuilder role(final Integer role) {
            this.role = role;
            return this;
        }

        /**
         * roles.
         *
         * @param roles the roles.
         * @return DashboardUserDTOBuilder.
         */
        public DashboardUserDTOBuilder roles(final List<String> roles) {
            this.roles = roles;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the userName.
         * @return DashboardUserDTOBuilder.
         */
        public DashboardUserDTOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * clientId.
         *
         * @param clientId the clientId.
         * @return DashboardUserDTOBuilder.
         */
        public DashboardUserDTOBuilder clientId(final String clientId) {
            this.clientId = clientId;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public DashboardUserDTO build() {
            return new DashboardUserDTO(id, userName, password, role, roles, enabled, clientId);
        }
    }
}
