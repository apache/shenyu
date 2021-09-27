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

import java.util.List;
import java.util.Objects;

/**
 * AppAuthDTO.
 *
 * @since 2.0.0
 */
public class AppAuthData {

    private String appKey;

    private String appSecret;

    private Boolean enabled;

    private Boolean open;

    private List<AuthParamData> paramDataList;

    private List<AuthPathData> pathDataList;

    /**
     * no args constructor.
     */
    public AppAuthData() {
    }

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private AppAuthData(final Builder builder) {
        this.appKey = builder.appKey;
        this.appSecret = builder.appSecret;
        this.enabled = builder.enabled;
        this.open = builder.open;
        this.paramDataList = builder.paramDataList;
        this.pathDataList = builder.pathDataList;
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
     * get appKey.
     *
     * @return appKey
     */
    public String getAppKey() {
        return appKey;
    }

    /**
     * set appKey.
     *
     * @param appKey appKey
     */
    public void setAppKey(final String appKey) {
        this.appKey = appKey;
    }

    /**
     * get appSecret.
     *
     * @return appSecret
     */
    public String getAppSecret() {
        return appSecret;
    }

    /**
     * set appSecret.
     *
     * @param appSecret appSecret
     */
    public void setAppSecret(final String appSecret) {
        this.appSecret = appSecret;
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

    /**
     * get open.
     *
     * @return open
     */
    public Boolean getOpen() {
        return open;
    }

    /**
     * set open.
     *
     * @param open open
     */
    public void setOpen(final Boolean open) {
        this.open = open;
    }

    /**
     * get paramDataList.
     *
     * @return paramDataList
     */
    public List<AuthParamData> getParamDataList() {
        return paramDataList;
    }

    /**
     * set paramDataList.
     *
     * @param paramDataList paramDataList
     */
    public void setParamDataList(final List<AuthParamData> paramDataList) {
        this.paramDataList = paramDataList;
    }

    /**
     * get pathDataList.
     *
     * @return pathDataList
     */
    public List<AuthPathData> getPathDataList() {
        return pathDataList;
    }

    /**
     * set pathDataList.
     *
     * @param pathDataList pathDataList
     */
    public void setPathDataList(final List<AuthPathData> pathDataList) {
        this.pathDataList = pathDataList;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        AppAuthData that = (AppAuthData) o;
        return Objects.equals(appKey, that.appKey) && Objects.equals(appSecret, that.appSecret) && Objects.equals(enabled, that.enabled)
                && Objects.equals(open, that.open) && Objects.equals(paramDataList, that.paramDataList) && Objects.equals(pathDataList, that.pathDataList);
    }

    @Override
    public int hashCode() {
        return Objects.hash(appKey, appSecret, enabled, open, paramDataList, pathDataList);
    }

    @Override
    public String toString() {
        return "AppAuthData{"
                + "appKey='"
                + appKey
                + '\''
                + ", appSecret='"
                + appSecret
                + '\''
                + ", enabled="
                + enabled
                + ", open="
                + open
                + ", paramDataList="
                + paramDataList
                + ", pathDataList="
                + pathDataList
                + '}';
    }

    /**
     * class builder.
     */
    public static final class Builder {

        /**
         * appKey.
         */
        private String appKey;

        /**
         * appSecret.
         */
        private String appSecret;

        /**
         * enabled.
         */
        private Boolean enabled;

        /**
         * open.
         */
        private Boolean open;

        /**
         * paramDataList.
         */
        private List<AuthParamData> paramDataList;

        /**
         * pathDataList.
         */
        private List<AuthPathData> pathDataList;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return AppAuthData
         */
        public AppAuthData build() {
            return new AppAuthData(this);
        }

        /**
         * build appKey.
         *
         * @param appKey appKey
         * @return this
         */
        public Builder appKey(final String appKey) {
            this.appKey = appKey;
            return this;
        }

        /**
         * build appSecret.
         *
         * @param appSecret appSecret
         * @return this
         */
        public Builder appSecret(final String appSecret) {
            this.appSecret = appSecret;
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

        /**
         * build open.
         *
         * @param open open
         * @return this
         */
        public Builder open(final Boolean open) {
            this.open = open;
            return this;
        }

        /**
         * build paramDataList.
         *
         * @param paramDataList paramDataList
         * @return this
         */
        public Builder paramDataList(final List<AuthParamData> paramDataList) {
            this.paramDataList = paramDataList;
            return this;
        }

        /**
         * build pathDataList.
         *
         * @param pathDataList pathDataList
         * @return this
         */
        public Builder pathDataList(final List<AuthPathData> pathDataList) {
            this.pathDataList = pathDataList;
            return this;
        }
    }
}
