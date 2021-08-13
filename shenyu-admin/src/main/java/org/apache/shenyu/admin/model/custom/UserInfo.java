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

package org.apache.shenyu.admin.model.custom;

/*
 * this is user info.
 */
public class UserInfo {

    /**
     * user name.
     */
    private String userName;

    /**
     * user id.
     */
    private String userId;

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
     * builder method.
     *
     * @return builder object.
     */
    public static UserInfo.UserInfoBuilder builder() {
        return new UserInfo.UserInfoBuilder();
    }

    public static final class UserInfoBuilder {

        private String userName;

        private String userId;

        private UserInfoBuilder() {
        }

        /**
         * userName.
         *
         * @param userName the userName.
         * @return UserInfoBuilder.
         */
        public UserInfoBuilder userName(final String userName) {
            this.userName = userName;
            return this;
        }

        /**
         * userId.
         *
         * @param userId the userId.
         * @return UserInfoBuilder.
         */
        public UserInfoBuilder userId(final String userId) {
            this.userId = userId;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public UserInfo build() {
            UserInfo userInfo = new UserInfo();
            userInfo.setUserName(userName);
            userInfo.setUserId(userId);
            return userInfo;
        }
    }
}
