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
import java.util.Objects;

/**
 * The type Auth param dto.
 */
public class AuthParamDTO implements Serializable {

    private static final long serialVersionUID = -5758853682062396957L;

    private String appName;

    private String appParam;

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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AuthParamDTO)) {
            return false;
        }
        AuthParamDTO that = (AuthParamDTO) o;
        return Objects.equals(appName, that.appName) && Objects.equals(appParam, that.appParam);
    }

    @Override
    public int hashCode() {
        return Objects.hash(appName, appParam);
    }
}
