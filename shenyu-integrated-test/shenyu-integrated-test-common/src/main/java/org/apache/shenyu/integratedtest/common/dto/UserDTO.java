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

package org.apache.shenyu.integratedtest.common.dto;

/**
 * The type User dto.
 */
public class UserDTO {

    private String userId;

    private String userName;
    
    public UserDTO() {
    }
    
    public UserDTO(final String userId, final String userName) {
        this.userId = userId;
        this.userName = userName;
    }
    
    /**
     * Get the userId.
     *
     * @return the userId
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Set the userId.
     *
     * @param userId userId
     */
    public void setUserId(final String userId) {
        this.userId = userId;
    }

    /**
     * Get the userName.
     *
     * @return userName
     */
    public String getUserName() {
        return userName;
    }

    /**
     * Set the userName.
     *
     * @param userName userName
     */
    public void setUserName(final String userName) {
        this.userName = userName;
    }
}
