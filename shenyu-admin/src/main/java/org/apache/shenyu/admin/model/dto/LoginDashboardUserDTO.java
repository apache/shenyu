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

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Objects;

/**
 * login dashboard user from by web front.
 */
public class LoginDashboardUserDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * user name.
     */
    @NotBlank
    private String userName;

    /**
     * user password (encrypted on the front-end).
     */
    @NotBlank
    private String password;

    /**
     * client id.
     */
    private String clientId;

    public LoginDashboardUserDTO() {
    }

    public LoginDashboardUserDTO(final String userName, final String password, final String clientId) {
        this.userName = userName;
        this.password = password;
        this.clientId = clientId;
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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof LoginDashboardUserDTO)) {
            return false;
        }
        LoginDashboardUserDTO that = (LoginDashboardUserDTO) o;
        return Objects.equals(userName, that.userName)
                && Objects.equals(password, that.password)
                && Objects.equals(clientId, that.clientId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(userName, password, clientId);
    }
}
