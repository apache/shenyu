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

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * The type Auth apply dto.
 */
public class AuthApplyDTO implements Serializable {

    private static final long serialVersionUID = -6769162031997427810L;

    private String appKey;

    @NotBlank(message = "userId cannot be empty!")
    private String userId;

    @Pattern(regexp = "\\+?\\d{7,11}", message = "number is illegal, length 7 to 11! e.g. +1234567 or 1234567")
    private String phone;

    @NotBlank(message = "appName cannot be empty!")
    private String appName;

    private String appParam;

    private String extInfo;

    private Boolean open;

    private List<String> pathList;

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
     * Gets the value of pathList.
     *
     * @return the value of pathList
     */
    public List<String> getPathList() {
        return pathList;
    }

    /**
     * Sets the pathList.
     *
     * @param pathList pathList
     */
    public void setPathList(final List<String> pathList) {
        this.pathList = pathList;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AuthApplyDTO)) {
            return false;
        }
        AuthApplyDTO that = (AuthApplyDTO) o;
        return Objects.equals(appKey, that.appKey)
                && Objects.equals(userId, that.userId)
                && Objects.equals(phone, that.phone)
                && Objects.equals(appName, that.appName)
                && Objects.equals(appParam, that.appParam)
                && Objects.equals(extInfo, that.extInfo)
                && Objects.equals(open, that.open)
                && Objects.equals(pathList, that.pathList);
    }

    @Override
    public int hashCode() {
        return Objects.hash(appKey, userId, phone, appName, appParam, extInfo, open, pathList);
    }
}
