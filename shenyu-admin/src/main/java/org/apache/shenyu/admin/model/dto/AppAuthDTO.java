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

import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
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
    @NotBlank(message = "app auth id not null")
    @Existed(message = "app auth is not existed", provider = AppAuthMapper.class)
    private String id;
    
    /**
     * application key.
     */
    @NotBlank(message = "app auth appKey not null")
    private String appKey;
    
    /**
     * encryption secret.
     */
    @NotBlank(message = "app auth appSecret not null")
    private String appSecret;
    
    private String userId;

    @Pattern(regexp = "\\+?\\d{7,11}", message = "number is illegal, length 7 to 11! e.g. +1234567 or 1234567")
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

    @Valid
    private List<AuthParamDTO> authParamList;

    @Valid
    private List<AuthPathDTO> authPathList;

    /**
     * namespaceId.
     */
    @NotBlank
    @Existed(message = "namespaceId is not existed", provider = NamespaceMapper.class)
    private String namespaceId;
    
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
    public List<AuthParamDTO> getAuthParamList() {
        return authParamList;
    }
    
    /**
     * Sets the authParamDTOList.
     *
     * @param authParamList authParamDTOList
     */
    public void setAuthParamList(final List<AuthParamDTO> authParamList) {
        this.authParamList = authParamList;
    }
    
    /**
     * Gets the value of authPathDTOList.
     *
     * @return the value of authPathDTOList
     */
    public List<AuthPathDTO> getAuthPathList() {
        return authPathList;
    }
    
    /**
     * Sets the authPathDTOList.
     *
     * @param authPathList authPathDTOList
     */
    public void setAuthPathList(final List<AuthPathDTO> authPathList) {
        this.authPathList = authPathList;
    }

    /**
     * get namespaceId.
     *
     * @return namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
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
                && Objects.equals(authParamList, that.authParamList)
                && Objects.equals(authPathList, that.authPathList)
                && Objects.equals(namespaceId, that.namespaceId);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(id, appKey, appSecret, userId, phone, extInfo, open, enabled, authParamList, authPathList, namespaceId);
    }
}
