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
import java.io.Serializable;
import java.util.Objects;

/**
 * The type Auth path apply dto.
 */
public class AuthPathApplyDTO implements Serializable {

    private static final long serialVersionUID = 6898559464876411474L;

    private String appName;

    @NotBlank
    private String path;

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
     * Gets the value of path.
     *
     * @return the value of path
     */
    public String getPath() {
        return path;
    }

    /**
     * Sets the path.
     *
     * @param path path
     */
    public void setPath(final String path) {
        this.path = path;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AuthPathApplyDTO)) {
            return false;
        }
        AuthPathApplyDTO that = (AuthPathApplyDTO) o;
        return Objects.equals(appName, that.appName) && Objects.equals(path, that.path);
    }

    @Override
    public int hashCode() {
        return Objects.hash(appName, path);
    }

    @Override
    public String toString() {
        return "AuthPathApplyDTO{"
                + "appName='" + appName + '\''
                + ", path='" + path + '\''
                + '}';
    }
}
