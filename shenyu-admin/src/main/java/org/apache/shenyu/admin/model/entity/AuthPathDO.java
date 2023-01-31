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
 * The type Auth path do.
 */
public final class AuthPathDO extends BaseDO {

    private static final long serialVersionUID = 2265360597468199607L;

    private String authId;

    private String appName;

    private String path;

    private Boolean enabled;

    public AuthPathDO() {
    }

    public AuthPathDO(final String authId, final String appName, final String path, final Boolean enabled) {
        this.authId = authId;
        this.appName = appName;
        this.path = path;
        this.enabled = enabled;
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
     * Gets the value of path.
     *
     * @return the value of path
     */
    public String getPath() {
        return path;
    }

    /**
     * Sets the path.
     *
     * @param path path
     */
    public void setPath(final String path) {
        this.path = path;
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
     * Build AuthPathDO object with given params.
     *
     * @param path      {@linkplain String}
     * @param authId    {@linkplain String}
     * @param appName   {@linkplain String}
     * @return          {@linkplain AuthPathDO}
     */
    public static AuthPathDO create(final String path, final String authId, final String appName) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        return AuthPathDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .authId(authId)
                .appName(appName)
                .path(path)
                .enabled(true)
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
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
        AuthPathDO that = (AuthPathDO) o;
        return Objects.equals(authId, that.authId) && Objects.equals(appName, that.appName) && Objects.equals(path, that.path) && Objects.equals(enabled, that.enabled);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), authId, appName, path, enabled);
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static AuthPathDO.AuthPathDOBuilder builder() {
        return new AuthPathDO.AuthPathDOBuilder();
    }

    public static final class AuthPathDOBuilder {

        private String authId;

        private String appName;

        private String path;

        private Boolean enabled;

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private AuthPathDOBuilder() {
        }

        /**
         * authId.
         *
         * @param authId the authId.
         * @return AuthPathDOBuilder.
         */
        public AuthPathDOBuilder authId(final String authId) {
            this.authId = authId;
            return this;
        }

        /**
         * appName.
         *
         * @param appName the appName.
         * @return AuthPathDOBuilder.
         */
        public AuthPathDOBuilder appName(final String appName) {
            this.appName = appName;
            return this;
        }

        /**
         * path.
         *
         * @param path the path.
         * @return AuthPathDOBuilder.
         */
        public AuthPathDOBuilder path(final String path) {
            this.path = path;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return AuthPathDOBuilder.
         */
        public AuthPathDOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * id.
         *
         * @param id the id.
         * @return AuthPathDOBuilder.
         */
        public AuthPathDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return AuthPathDOBuilder.
         */
        public AuthPathDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return AuthPathDOBuilder.
         */
        public AuthPathDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public AuthPathDO build() {
            AuthPathDO authPathDO = new AuthPathDO();
            authPathDO.setAuthId(authId);
            authPathDO.setAppName(appName);
            authPathDO.setPath(path);
            authPathDO.setEnabled(enabled);
            authPathDO.setId(id);
            authPathDO.setDateCreated(dateCreated);
            authPathDO.setDateUpdated(dateUpdated);
            return authPathDO;
        }
    }
}
