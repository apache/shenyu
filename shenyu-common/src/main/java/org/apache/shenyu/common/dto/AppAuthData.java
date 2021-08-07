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

    private static final long serialVersionUID = -7060944416765128601L;

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
     * all args constructor.
     *
     * @param appKey        appKey
     * @param appSecret     appSecret
     * @param enabled       enabled
     * @param open          open
     * @param paramDataList paramDataList
     * @param pathDataList  pathDataList
     */
    public AppAuthData(final String appKey, final String appSecret, final Boolean enabled, final Boolean open, final List<AuthParamData> paramDataList, final List<AuthPathData> pathDataList) {
        this.appKey = appKey;
        this.appSecret = appSecret;
        this.enabled = enabled;
        this.open = open;
        this.paramDataList = paramDataList;
        this.pathDataList = pathDataList;
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
        return Objects.equals(appKey, that.appKey)
                && Objects.equals(appSecret, that.appSecret)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(open, that.open)
                && Objects.equals(paramDataList, that.paramDataList)
                && Objects.equals(pathDataList, that.pathDataList);
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
}
