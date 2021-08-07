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

package org.apache.shenyu.admin.model.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * this is application authority from by web front.
 */
public class AppAuthDTO implements Serializable {

    private static final long serialVersionUID = 3906547569699874743L;

    /**
     * primary key.
     */
    private String id;

    /**
     * application key.
     */
    private String appKey;

    /**
     * encryption secret.
     */
    private String appSecret;

    private String userId;

    private String phone;

    private String extInfo;

    /**
     * whether open authPath.
     */
    private Boolean open;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    private List<AuthParamDTO> authParamDTOList;

    private List<AuthPathDTO> authPathDTOList;

    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
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
     * Gets the value of authParamDTOList.
     *
     * @return the value of authParamDTOList
     */
    public List<AuthParamDTO> getAuthParamDTOList() {
        return authParamDTOList;
    }

    /**
     * Sets the authParamDTOList.
     *
     * @param authParamDTOList authParamDTOList
     */
    public void setAuthParamDTOList(final List<AuthParamDTO> authParamDTOList) {
        this.authParamDTOList = authParamDTOList;
    }

    /**
     * Gets the value of authPathDTOList.
     *
     * @return the value of authPathDTOList
     */
    public List<AuthPathDTO> getAuthPathDTOList() {
        return authPathDTOList;
    }

    /**
     * Sets the authPathDTOList.
     *
     * @param authPathDTOList authPathDTOList
     */
    public void setAuthPathDTOList(final List<AuthPathDTO> authPathDTOList) {
        this.authPathDTOList = authPathDTOList;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AppAuthDTO)) {
            return false;
        }
        AppAuthDTO that = (AppAuthDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(appKey, that.appKey)
                && Objects.equals(appSecret, that.appSecret)
                && Objects.equals(userId, that.userId)
                && Objects.equals(phone, that.phone)
                && Objects.equals(extInfo, that.extInfo)
                && Objects.equals(open, that.open)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(authParamDTOList, that.authParamDTOList)
                && Objects.equals(authPathDTOList, that.authPathDTOList);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, appKey, appSecret, userId, phone, extInfo, open, enabled, authParamDTOList, authPathDTOList);
    }
}
