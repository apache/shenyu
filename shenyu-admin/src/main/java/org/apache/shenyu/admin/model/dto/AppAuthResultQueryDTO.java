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
import jakarta.validation.constraints.Pattern;
import java.io.Serializable;

/**
 * Result query: read-only lookup of the caller's own triple-key request.
 */
public class AppAuthResultQueryDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    private static final String ASCII_VISIBLE_MAX_64 = "^[\\x21-\\x7E]{1,64}$";

    /**
     * namespace.
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
