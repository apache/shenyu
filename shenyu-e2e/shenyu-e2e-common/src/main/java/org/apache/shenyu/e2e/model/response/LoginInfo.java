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

package org.apache.shenyu.e2e.model.response;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Login information.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class LoginInfo {
    
    @JsonAlias("userName")
    private String username;
    
    private String token;
    
    private boolean enabled;
    
    private long expiredTime;

    /**
     * get username.
     *
     * @return username
     */
    public String getUsername() {
        return username;
    }

    /**
     * set username.
     *
     * @param username username
     */
    public void setUsername(final String username) {
        this.username = username;
    }

    /**
     * get token.
     *
     * @return token
     */
    public String getToken() {
        return token;
    }

    /**
     * set token.
     *
     * @param token token
     */
    public void setToken(final String token) {
        this.token = token;
    }

    /**
     * is enabled.
     *
     * @return enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * set enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * get expiredTime.
     *
     * @return expiredTime
     */
    public long getExpiredTime() {
        return expiredTime;
    }

    /**
     * set expiredTime.
     *
     * @param expiredTime expiredTime
     */
    public void setExpiredTime(final long expiredTime) {
        this.expiredTime = expiredTime;
    }
}
