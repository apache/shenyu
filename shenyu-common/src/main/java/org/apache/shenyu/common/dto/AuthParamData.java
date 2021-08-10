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

import java.util.Objects;

/**
 * The type Auth param data.
 */
public class AuthParamData {

    private String appName;

    private String appParam;

    /**
     * no args constructor.
     */
    public AuthParamData() {
    }

    /**
     * all args constructor.
     *
     * @param appName  appName
     * @param appParam appParam
     */
    public AuthParamData(final String appName, final String appParam) {
        this.appName = appName;
        this.appParam = appParam;
    }

    /**
     * get appName.
     *
     * @return appName
     */
    public String getAppName() {
        return appName;
    }

    /**
     * set appName.
     *
     * @param appName appName
     */
    public void setAppName(final String appName) {
        this.appName = appName;
    }

    /**
     * get appParam.
     *
     * @return appParam
     */
    public String getAppParam() {
        return appParam;
    }

    /**
     * set appParam.
     *
     * @param appParam appParam
     */
    public void setAppParam(final String appParam) {
        this.appParam = appParam;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        AuthParamData that = (AuthParamData) o;
        return Objects.equals(appName, that.appName) && Objects.equals(appParam, that.appParam);
    }

    @Override
    public int hashCode() {
        return Objects.hash(appName, appParam);
    }

    @Override
    public String toString() {
        return "AuthParamData{"
                + "appName='"
                + appName
                + '\''
                + ", appParam='"
                + appParam
                + '\''
                + '}';
    }
}
