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

package org.apache.shenyu.plugin.basic.auth.config;

import java.io.Serializable;

/**
 * basic-auth Plugin.
 */
public class BasicAuthConfig implements Serializable {
    /**
     * private key.
     */
    private String username;

    /**
     * private password.
     */
    private String password;

    /**
     * get username.
     *
     * @return username.
     */
    public String getUsername() {
        return username;
    }

    /**
     * get password.
     *
     * @return password.
     */
    public String getPassword() {
        return password;
    }

    /**
     * set username.
     *
     * @param username username.
     */
    public void setUsername(final String username) {
        this.username = username;
    }

    /**
     * set password.
     *
     * @param password password.
     */
    public void setPassword(final String password) {
        this.password = password;
    }
}
