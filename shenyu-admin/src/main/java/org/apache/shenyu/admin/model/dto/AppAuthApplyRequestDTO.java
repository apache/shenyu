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

import com.fasterxml.jackson.annotation.JsonAnySetter;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.util.List;

/**
 * Apply request for the app auth lifecycle API.
 * Neither caller/appKey/appSecret are accepted (identity derives from the authenticated session and credentials
 * are server generated); open/enabled are server-fixed (open=true, enabled=false) and rejected if present.
 */
public class AppAuthApplyRequestDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    private static final String ASCII_VISIBLE_MAX_64 = "^[\\x21-\\x7E]{1,64}$";

    /**
     * Namespace id (visible ASCII, no whitespace).
     */
    @NotBlank(message = "namespaceId must not be blank")
    @Pattern(regexp = ASCII_VISIBLE_MAX_64, message = "namespaceId must be 1-64 visible ASCII characters")
    private String namespaceId;

    /**
     * Caller-generated idempotent request id.
     */
    @NotBlank(message = "requestId must not be blank")
    @Pattern(regexp = ASCII_VISIBLE_MAX_64, message = "requestId must be 1-64 visible ASCII characters")
    private String requestId;

    /**
     * Application name (business appKey semantics, not the auth appKey).
     */
    @NotBlank(message = "appName must not be blank")
    @Size(max = 255, message = "appName length must not exceed 255")
    private String appName;

    /**
     * Immutable user id of the authenticated service identity (ownership checked server side).
     */
    @Size(max = 64, message = "userId length must not exceed 64")
    private String userId;

    /**
     * Contact phone.
     */
    @Size(max = 32, message = "phone length must not exceed 32")
    private String phone;

    /**
     * Application parameters (same semantics as AuthApplyDTO).
     */
    private String appParam;

    /**
     * Extension info (same semantics as AuthApplyDTO).
     */
    private String extInfo;

    /**
     * Authorized paths: must not be null; an empty array means zero authorization until replacePaths fills it.
     */
    @NotNull(message = "pathList must not be null (empty array allowed)")
    private List<String> pathList;


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
     * Gets the value of requestId.
     *
     * @return the value of requestId
     */
    public String getRequestId() {
        return requestId;
    }

    /**
     * Sets the requestId.
     *
     * @param requestId requestId
     */
    public void setRequestId(final String requestId) {
        this.requestId = requestId;
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

    /**
     * Unknown fields (including forged caller/appKey/appSecret) are always rejected, never silently ignored.
     *
     * @param name unknown field name
     * @param value unknown field value
     */
    @JsonAnySetter
    private void rejectUnknownField(final String name, final Object value) {
        throw new IllegalArgumentException("Request body contains an unaccepted field: " + name);
    }
}
