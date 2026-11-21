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

/**
 * Application authority list view without the application secret.
 */
public class AppAuthListVO implements Serializable {

    private static final long serialVersionUID = -4017739201784333892L;

    /**
     * primary key.
     */
    private String id;

    /**
     * application key.
     */
    private String appKey;

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

    private String namespaceId;

    /**
     * updated time.
     */
    private String dateUpdated;

    public AppAuthListVO() {
    }

    public AppAuthListVO(final String id,
                         final String appKey,
                         final String userId,
                         final String phone,
                         final String extInfo,
                         final Boolean open,
                         final Boolean enabled,
                         final String namespaceId,
                         final String dateUpdated) {
        this.id = id;
        this.appKey = appKey;
        this.userId = userId;
        this.phone = phone;
        this.extInfo = extInfo;
        this.open = open;
        this.enabled = enabled;
        this.namespaceId = namespaceId;
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
     * Gets the value of namespaceId.
     *
     * @return the value of namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * Sets the namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
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
