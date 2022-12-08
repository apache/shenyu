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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.AppAuthDTO;
import org.apache.shenyu.admin.model.dto.AuthApplyDTO;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * AppAuthDO.
 */
public final class AppAuthDO extends BaseDO {

    private static final long serialVersionUID = 5683408559456006830L;

    /**
     * application key.
     */
    private String appKey;

    /**
     * encryption secret.
     */
    private String appSecret;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether open authPath.
     */
    private Boolean open;

    private String userId;

    private String phone;

    private String extInfo;

    public AppAuthDO() {
    }

    public AppAuthDO(final String appKey, final String appSecret, final Boolean enabled, final Boolean open, final String userId, final String phone, final String extInfo) {
        this.appKey = appKey;
        this.appSecret = appSecret;
        this.enabled = enabled;
        this.open = open;
        this.userId = userId;
        this.phone = phone;
        this.extInfo = extInfo;
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
     * Sets the appKey.
     *
     * @param appKey appKey
     */
    public void setAppKey(final String appKey) {
        this.appKey = appKey;
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
     * Sets the appSecret.
     *
     * @param appSecret appSecret
     */
    public void setAppSecret(final String appSecret) {
        this.appSecret = appSecret;
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
     * Gets the value of open.
     *
     * @return the value of open
     */
    public Boolean getOpen() {
        return open;
    }

    /**
     * Sets the open.
     *
     * @param open open
     */
    public void setOpen(final Boolean open) {
        this.open = open;
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
     * Gets the value of phone.
     *
     * @return the value of phone
     */
    public String getPhone() {
        return phone;
    }

    /**
     * Sets the phone.
     *
     * @param phone phone
     */
    public void setPhone(final String phone) {
        this.phone = phone;
    }

    /**
     * Gets the value of extInfo.
     *
     * @return the value of extInfo
     */
    public String getExtInfo() {
        return extInfo;
    }

    /**
     * Sets the extInfo.
     *
     * @param extInfo extInfo
     */
    public void setExtInfo(final String extInfo) {
        this.extInfo = extInfo;
    }

    /**
     * Build AppAuthDO object with given AppAuthDTO object.
     *
     * @param appAuthDTO {@linkplain AppAuthDTO}
     * @return {@linkplain AppAuthDO}
     */
    public static AppAuthDO create(final AppAuthDTO appAuthDTO) {
        return Optional.ofNullable(appAuthDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            AppAuthDO appAuthDO = AppAuthDO.builder()
                    .appKey(item.getAppKey())
                    .appSecret(item.getAppSecret())
                    .open(item.getOpen())
                    .enabled(item.getEnabled())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                appAuthDO.setId(UUIDUtils.getInstance().generateShortUuid());
                appAuthDO.setDateCreated(currentTime);
            } else {
                appAuthDO.setId(item.getId());
            }
            return appAuthDO;
        }).orElse(null);
    }

    /**
     * Build AppAuthDO object with given AuthApplyDTO object.
     *
     * @param authApplyDTO {@linkplain AuthApplyDTO}
     * @return {@linkplain AppAuthDO}
     */
    public static AppAuthDO create(final AuthApplyDTO authApplyDTO) {
        return Optional.ofNullable(authApplyDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            return AppAuthDO.builder()
                    .id(UUIDUtils.getInstance().generateShortUuid())
                    .userId(item.getUserId())
                    .phone(item.getPhone())
                    .extInfo(item.getExtInfo())
                    .appKey(SignUtils.generateKey())
                    .appSecret(SignUtils.generateKey())
                    .open(item.getOpen())
                    .enabled(true)
                    .dateCreated(currentTime)
                    .dateUpdated(currentTime)
                    .build();
        }).orElse(null);
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static AppAuthDO.AppAuthDOBuilder builder() {
        return new AppAuthDO.AppAuthDOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AppAuthDO)) {
            return false;
        }
        AppAuthDO appAuthDO = (AppAuthDO) o;
        if (!super.equals(o)) {
            return false;
        }
        return appKey.equals(appAuthDO.appKey)
                && appSecret.equals(appAuthDO.appSecret)
                && enabled.equals(appAuthDO.enabled)
                && open.equals(appAuthDO.open)
                && userId.equals(appAuthDO.userId)
                && phone.equals(appAuthDO.phone)
                && extInfo.equals(appAuthDO.extInfo);
    }

    @Override
    public int hashCode() {
        return Objects.hash(appKey, appSecret, enabled, open, userId, phone, extInfo);
    }

    public static final class AppAuthDOBuilder {

        private String appKey;

        private String appSecret;

        private Boolean enabled;

        private Boolean open;

        private String userId;

        private String phone;

        private String extInfo;

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private AppAuthDOBuilder() {
        }

        /**
         * appkey.
         *
         * @param appKey the appKey.
         * @return AppAuthDOBuilder.
         */
        public AppAuthDOBuilder appKey(final String appKey) {
            this.appKey = appKey;
            return this;
        }

        /**
         * appSecret.
         *
         * @param appSecret the appSecret.
         * @return AppAuthDOBuilder.
         */
        public AppAuthDOBuilder appSecret(final String appSecret) {
            this.appSecret = appSecret;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return AppAuthDOBuilder.
         */
        public AppAuthDOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * open.
         *
         * @param open the open.
         * @return AppAuthDOBuilder.
         */
        public AppAuthDOBuilder open(final Boolean open) {
            this.open = open;
            return this;
        }

        /**
         * userId.
         *
         * @param userId the userId.
         * @return AppAuthDOBuilder.
         */
        public AppAuthDOBuilder userId(final String userId) {
            this.userId = userId;
            return this;
        }

        /**
         * phone.
         *
         * @param phone the phone.
         * @return AppAuthDOBuilder.
         */
        public AppAuthDOBuilder phone(final String phone) {
            this.phone = phone;
            return this;
        }

        /**
         * extInfo.
         *
         * @param extInfo the extInfo.
         * @return AppAuthDOBuilder.
         */
        public AppAuthDOBuilder extInfo(final String extInfo) {
            this.extInfo = extInfo;
            return this;
        }

        /**
         * id.
         *
         * @param id the id.
         * @return AppAuthDOBuilder.
         */
        public AppAuthDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return AppAuthDOBuilder.
         */
        public AppAuthDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return AppAuthDOBuilder.
         */
        public AppAuthDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public AppAuthDO build() {
            AppAuthDO appAuthDO = new AppAuthDO();
            appAuthDO.setAppKey(appKey);
            appAuthDO.setAppSecret(appSecret);
            appAuthDO.setEnabled(enabled);
            appAuthDO.setOpen(open);
            appAuthDO.setUserId(userId);
            appAuthDO.setPhone(phone);
            appAuthDO.setExtInfo(extInfo);
            appAuthDO.setId(id);
            appAuthDO.setDateCreated(dateCreated);
            appAuthDO.setDateUpdated(dateUpdated);
            return appAuthDO;
        }
    }
}
