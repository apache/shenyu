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

/**
 * login dashboard return user info's vo.
 */
public class LoginDashboardUserVO extends DashboardUserVO {

    private static final long serialVersionUID = -411996250594776944L;

    /**
     * token.
     */
    private String token;

    /**
     * expired time(milliSeconds).
     */
    private Long expiredTime;

    public LoginDashboardUserVO() {
    }

    /**
     * Gets the value of token.
     *
     * @return the value of token
     */
    public String getToken() {
        return token;
    }

    /**
     * Sets the token.
     *
     * @param token token
     * @return {@link LoginDashboardUserVO}
     */
    public LoginDashboardUserVO setToken(final String token) {
        this.token = token;
        return this;
    }

    /**
     * Gets the value of expiredTime.
     *
     * @return the value of expiredTime
     */
    public Long getExpiredTime() {
        return expiredTime;
    }

    /**
     * Sets the expiredTime.
     *
     * @param expiredTime expiredTime
     * @return {@link LoginDashboardUserVO}
     */
    public LoginDashboardUserVO setExpiredTime(final Long expiredTime) {
        this.expiredTime = expiredTime;
        return this;
    }

    /**
     * build loginDashboardUserVO.
     *
     * @param dashboardUserVO {@linkplain DashboardUserVO}
     * @return {@linkplain LoginDashboardUserVO}
     */
    public static LoginDashboardUserVO buildLoginDashboardUserVO(final DashboardUserVO dashboardUserVO) {
        return DashboardUserTransfer.INSTANCE.transferVO2LoginVO(dashboardUserVO);
    }
}
