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

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * X1 result view object.
 * State: NOT_FOUND / CREATED / REVOKED; only CREATED carries id/appKey/appSecret/enabled/open/pathList,
 * other states never carry credential fields.
 */
public final class AppAuthResultVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * NOT_FOUND / CREATED / REVOKED.
     */
    private String state;

    /**
     * Idempotency triple-key echo.
     */
    private String namespaceId;

    /**
     * Idempotency triple-key echo.
     */
    private String requestId;

    /**
     * Auth id (CREATED only).
     */
    private String id;

    /**
     * Auth appKey (CREATED only).
     */
    private String appKey;

    /**
     * Auth appSecret (CREATED only; returned to the authorized caller only, never logged).
     */
    private String appSecret;

    /**
     * Current enabled state (CREATED only).
     */
    private Boolean enabled;

    /**
     * Always true (CREATED only).
     */
    private Boolean open;

    /**
     * Current authorized paths (CREATED only).
     */
    private List<String> pathList;

    /**
     * Creates a new builder.
     *
     * @return builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Gets the value of state.
     *
     * @return the value of state
     */
    public String getState() {
        return state;
    }

    /**
     * Gets the value of namespaceId.
     *
     * @return the value of namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * Gets the value of requestId.
     *
     * @return the value of requestId
     */
    public String getRequestId() {
        return requestId;
    }

    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Gets the value of appKey.
     *
     * @return the value of appKey
     */
    public String getAppKey() {
        return appKey;
    }

    /**
     * Gets the value of appSecret.
     *
     * @return the value of appSecret
     */
    public String getAppSecret() {
        return appSecret;
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
     * Gets the value of open.
     *
     * @return the value of open
     */
    public Boolean getOpen() {
        return open;
    }

    /**
     * Gets the value of pathList.
     *
     * @return the value of pathList
     */
    public List<String> getPathList() {
        return pathList;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        final AppAuthResultVO that = (AppAuthResultVO) o;
        return Objects.equals(state, that.state) && Objects.equals(id, that.id)
                && Objects.equals(requestId, that.requestId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(state, id, requestId);
    }

    @Override
    public String toString() {
        // appSecret is never printed
        return "AppAuthResultVO{state='" + state + "', namespaceId='" + namespaceId
                + "', requestId='" + requestId + "', id='" + id + "', appKey='" + appKey
                + "', enabled=" + enabled + ", open=" + open + ", pathList=" + pathList + "}";
    }

    /**
     * Builder for AppAuthResultVO.
     */
    public static final class Builder {

        private String state;

        private String namespaceId;

        private String requestId;

        private String id;

        private String appKey;

        private String appSecret;

        private Boolean enabled;

        private Boolean open;

        private List<String> pathList;

        /**
         * Sets the state.
         *
         * @param state state
         * @return this builder
         */
        public Builder state(final String state) {
            this.state = state;
            return this;
        }

        /**
         * Sets the namespaceId.
         *
         * @param namespaceId namespaceId
         * @return this builder
         */
        public Builder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        /**
         * Sets the requestId.
         *
         * @param requestId requestId
         * @return this builder
         */
        public Builder requestId(final String requestId) {
            this.requestId = requestId;
            return this;
        }

        /**
         * Sets the id.
         *
         * @param id id
         * @return this builder
         */
        public Builder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * Sets the appKey.
         *
         * @param appKey appKey
         * @return this builder
         */
        public Builder appKey(final String appKey) {
            this.appKey = appKey;
            return this;
        }

        /**
         * Sets the appSecret.
         *
         * @param appSecret appSecret
         * @return this builder
         */
        public Builder appSecret(final String appSecret) {
            this.appSecret = appSecret;
            return this;
        }

        /**
         * Sets the enabled.
         *
         * @param enabled enabled
         * @return this builder
         */
        public Builder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * Sets the open.
         *
         * @param open open
         * @return this builder
         */
        public Builder open(final Boolean open) {
            this.open = open;
            return this;
        }

        /**
         * Sets the pathList.
         *
         * @param pathList pathList
         * @return this builder
         */
        public Builder pathList(final List<String> pathList) {
            this.pathList = pathList;
            return this;
        }

        /**
         * Builds the VO.
         *
         * @return AppAuthResultVO
         */
        public AppAuthResultVO build() {
            final AppAuthResultVO vo = new AppAuthResultVO();
            vo.state = this.state;
            vo.namespaceId = this.namespaceId;
            vo.requestId = this.requestId;
            vo.id = this.id;
            vo.appKey = this.appKey;
            vo.appSecret = this.appSecret;
            vo.enabled = this.enabled;
            vo.open = this.open;
            vo.pathList = this.pathList;
            return vo;
        }
    }

}
