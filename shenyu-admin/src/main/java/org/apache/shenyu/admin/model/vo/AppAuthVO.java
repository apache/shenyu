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

/**
 * this is application authority view to web front.
 */
public class AppAuthVO implements Serializable {

    private static final long serialVersionUID = -3783316657677071171L;

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

    private List<AuthParamVO> authParamVOList;

    private List<AuthPathVO> authPathVOList;

    /**
     * updated time.
     */
    private String dateUpdated;

    public AppAuthVO() {
    }

    public AppAuthVO(final String id,
                     final String appKey,
                     final String appSecret,
                     final String userId,
                     final String phone,
                     final String extInfo,
                     final Boolean open,
                     final Boolean enabled,
                     final List<AuthParamVO> authParamVOList,
                     final List<AuthPathVO> authPathVOList,
                     final String dateUpdated) {
        this.id = id;
        this.appKey = appKey;
        this.appSecret = appSecret;
        this.userId = userId;
        this.phone = phone;
        this.extInfo = extInfo;
        this.open = open;
        this.enabled = enabled;
        this.authParamVOList = authParamVOList;
        this.authPathVOList = authPathVOList;
        this.dateUpdated = dateUpdated;
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
     * Gets the value of authParamVOList.
     *
     * @return the value of authParamVOList
     */
    public List<AuthParamVO> getAuthParamVOList() {
        return authParamVOList;
    }

    /**
     * Sets the authParamVOList.
     *
     * @param authParamVOList authParamVOList
     */
    public void setAuthParamVOList(final List<AuthParamVO> authParamVOList) {
        this.authParamVOList = authParamVOList;
    }

    /**
     * Gets the value of authPathVOList.
     *
     * @return the value of authPathVOList
     */
    public List<AuthPathVO> getAuthPathVOList() {
        return authPathVOList;
    }

    /**
     * Sets the authPathVOList.
     *
     * @param authPathVOList authPathVOList
     */
    public void setAuthPathVOList(final List<AuthPathVO> authPathVOList) {
        this.authPathVOList = authPathVOList;
    }

    /**
     * Gets the value of dateUpdated.
     *
     * @return the value of dateUpdated
     */
    public String getDateUpdated() {
        return dateUpdated;
    }

    /**
     * Sets the dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }
}
