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

import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Objects;

/**
 * The type Auth param do.
 */
public final class AuthParamDO extends BaseDO {

    private static final long serialVersionUID = -6719996683886817375L;

    private String authId;

    private String appName;

    private String appParam;

    public AuthParamDO() {
    }

    public AuthParamDO(final String authId, final String appName, final String appParam) {
        this.authId = authId;
        this.appName = appName;
        this.appParam = appParam;
    }

    /**
     * Gets the value of authId.
     *
     * @return the value of authId
     */
    public String getAuthId() {
        return authId;
    }

    /**
     * Sets the authId.
     *
     * @param authId authId
     */
    public void setAuthId(final String authId) {
        this.authId = authId;
    }

    /**
     * Gets the value of appName.
     *
     * @return the value of appName
     */
    public String getAppName() {
        return appName;
    }

    /**
     * Sets the appName.
     *
     * @param appName appName
     */
    public void setAppName(final String appName) {
        this.appName = appName;
    }

    /**
     * Gets the value of appParam.
     *
     * @return the value of appParam
     */
    public String getAppParam() {
        return appParam;
    }

    /**
     * Sets the appParam.
     *
     * @param appParam appParam
     */
    public void setAppParam(final String appParam) {
        this.appParam = appParam;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static AuthParamDO.AuthParamDOBuilder builder() {
        return new AuthParamDO.AuthParamDOBuilder();
    }

    /**
     * Build AuthParamDO object with given params.
     *
     * @param authId    {@linkplain String}
     * @param appName   {@linkplain String}
     * @param appParam  {@linkplain String}
     * @return          {@linkplain AuthParamDO}
     */
    public static AuthParamDO create(final String authId, final String appName, final String appParam) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        return AuthParamDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .authId(authId)
                .appName(appName)
                .appParam(appParam)
                .dateUpdated(currentTime)
                .dateCreated(currentTime)
                .build();
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
        AuthParamDO that = (AuthParamDO) o;
        return Objects.equals(authId, that.authId) && Objects.equals(appName, that.appName) && Objects.equals(appParam, that.appParam);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), authId, appName, appParam);
    }

    public static final class AuthParamDOBuilder {

        private String authId;

        private String appName;

        private String appParam;

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private AuthParamDOBuilder() {
        }

        /**
         * authId.
         *
         * @param authId the authId.
         * @return AuthParamDOBuilder.
         */
        public AuthParamDOBuilder authId(final String authId) {
            this.authId = authId;
            return this;
        }

        /**
         * appName.
         *
         * @param appName the appName.
         * @return AuthParamDOBuilder.
         */
        public AuthParamDOBuilder appName(final String appName) {
            this.appName = appName;
            return this;
        }

        /**
         * appParam.
         *
         * @param appParam the appParam.
         * @return AuthParamDOBuilder.
         */
        public AuthParamDOBuilder appParam(final String appParam) {
            this.appParam = appParam;
            return this;
        }

        /**
         * id.
         *
         * @param id the id.
         * @return AuthParamDOBuilder.
         */
        public AuthParamDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return AuthParamDOBuilder.
         */
        public AuthParamDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return AuthParamDOBuilder.
         */
        public AuthParamDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public AuthParamDO build() {
            AuthParamDO authParamDO = new AuthParamDO();
            authParamDO.setAuthId(authId);
            authParamDO.setAppName(appName);
            authParamDO.setAppParam(appParam);
            authParamDO.setId(id);
            authParamDO.setDateCreated(dateCreated);
            authParamDO.setDateUpdated(dateUpdated);
            return authParamDO;
        }
    }
}
