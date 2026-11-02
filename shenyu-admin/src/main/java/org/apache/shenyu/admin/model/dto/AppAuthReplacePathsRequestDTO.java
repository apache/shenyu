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
 * ReplacePaths request: replaces the managed auth paths as a whole; pathList must not be null and an empty array clears (real de-authorization).
 */
public class AppAuthReplacePathsRequestDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    private static final String ASCII_VISIBLE_MAX_64 = "^[\\x21-\\x7E]{1,64}$";

    /**
     * namespace.
     */
    @NotBlank(message = "namespaceId must not be blank")
    @Pattern(regexp = ASCII_VISIBLE_MAX_64, message = "namespaceId must be 1-64 visible ASCII characters")
    private String namespaceId;

    /**
     * Target auth id (used after ownership validation).
     */
    @NotBlank(message = "authId must not be blank")
    @Size(max = 255, message = "authId length must not exceed 255")
    private String authId;

    /**
     * New path set: not null; an empty array clears all authorized paths.
     */
    @NotNull(message = "pathList must not be null (empty array means clearing all paths)")
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
     * Gets the value of authId.
     *
     * @return the value of authId
     */
    public String getAuthId() {
        return authId;
    }

    /**
     * Sets the authId.
     *
     * @param authId authId
     */
    public void setAuthId(final String authId) {
        this.authId = authId;
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
     * Unknown fields are always rejected.
     *
     * @param name unknown field name
     * @param value unknown field value
     */
    @JsonAnySetter
    private void rejectUnknownField(final String name, final Object value) {
        throw new IllegalArgumentException("Request body contains an unaccepted field: " + name);
    }
}
