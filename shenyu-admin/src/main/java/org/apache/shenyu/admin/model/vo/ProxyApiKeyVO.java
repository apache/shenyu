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
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * ProxyApiKeyVO for web layer.
 */
public class ProxyApiKeyVO implements Serializable {

    private static final long serialVersionUID = -8123456789012345678L;

    private String id;

    private String proxyApiKey;

    private String description;

    private Boolean enabled;

    private String namespaceId;

    private String selectorId;

    /**
     * updated time for display.
     */
    private String dateUpdated;

    /**
     * Only used in detail view; avoid returning in list by default.
     */
    @JsonInclude(Include.NON_NULL)
    private String realApiKey;

    public String getId() {
        return id;
    }

    public void setId(final String id) {
        this.id = id;
    }

    public String getProxyApiKey() {
        return proxyApiKey;
    }

    public void setProxyApiKey(final String proxyApiKey) {
        this.proxyApiKey = proxyApiKey;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(final String description) {
        this.description = description;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    public String getNamespaceId() {
        return namespaceId;
    }

    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    public String getSelectorId() {
        return selectorId;
    }

    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }

    public String getDateUpdated() {
        return dateUpdated;
    }

    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    public String getRealApiKey() {
        return realApiKey;
    }

    public void setRealApiKey(final String realApiKey) {
        this.realApiKey = realApiKey;
    }
}