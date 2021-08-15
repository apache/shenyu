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

package org.apache.shenyu.common.dto;

import java.util.Objects;

/**
 * The type Auth path data.
 */
public class AuthPathData {

    private String appName;

    private String path;

    private Boolean enabled;

    /**
     * no args constructor.
     */
    public AuthPathData() {
    }

    /**
     * all args constructor.
     *
     * @param appName appName
     * @param path    path
     * @param enabled enabled
     */
    public AuthPathData(final String appName, final String path, final Boolean enabled) {
        this.appName = appName;
        this.path = path;
        this.enabled = enabled;
    }

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private AuthPathData(final Builder builder) {
        this.appName = builder.appName;
        this.path = builder.path;
        this.enabled = builder.enabled;
    }

    /**
     * class builder.
     *
     * @return builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * get appName.
     *
     * @return appName
     */
    public String getAppName() {
        return appName;
    }

    /**
     * set appName.
     *
     * @param appName appName
     */
    public void setAppName(final String appName) {
        this.appName = appName;
    }

    /**
     * get path.
     *
     * @return path
     */
    public String getPath() {
        return path;
    }

    /**
     * set path.
     *
     * @param path path
     */
    public void setPath(final String path) {
        this.path = path;
    }

    /**
     * get enabled.
     *
     * @return enabled
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * set enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        AuthPathData that = (AuthPathData) o;
        return Objects.equals(appName, that.appName) && Objects.equals(path, that.path) && Objects.equals(enabled, that.enabled);
    }

    @Override
    public int hashCode() {
        return Objects.hash(appName, path, enabled);
    }

    @Override
    public String toString() {
        return "AuthPathData{"
                + "appName='"
                + appName
                + '\''
                + ", path='"
                + path
                + '\''
                + ", enabled="
                + enabled
                + '}';
    }

    /**
     * class builder.
     */
    public static final class Builder {

        /**
         * appName.
         */
        private String appName;

        /**
         * path.
         */
        private String path;

        /**
         * enabled.
         */
        private Boolean enabled;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return AuthPathData
         */
        public AuthPathData build() {
            return new AuthPathData(this);
        }

        /**
         * build appName.
         *
         * @param appName appName
         * @return this
         */
        public Builder appName(final String appName) {
            this.appName = appName;
            return this;
        }

        /**
         * build path.
         *
         * @param path path
         * @return this
         */
        public Builder path(final String path) {
            this.path = path;
            return this;
        }

        /**
         * build enabled.
         *
         * @param enabled enabled
         * @return this
         */
        public Builder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }
    }
}
