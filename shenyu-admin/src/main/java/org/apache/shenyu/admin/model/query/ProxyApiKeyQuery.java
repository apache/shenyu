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

package org.apache.shenyu.admin.model.query;

import org.apache.shenyu.admin.model.page.PageParameter;

import java.io.Serializable;
import java.util.Objects;

/**
 * ProxyApiKeyQuery.
 */
public class ProxyApiKeyQuery implements Serializable {

    private static final long serialVersionUID = 884562341234567890L;

    private String proxyApiKey;

    private Boolean enabled;

    private PageParameter pageParameter;

    private String namespaceId;

    /**
     * The selector id used to narrow results to keys under the same namespace as the selector.
     */
    private String selectorId;

    public ProxyApiKeyQuery() {
    }

    public ProxyApiKeyQuery(final String proxyApiKey, final Boolean enabled, final PageParameter pageParameter,
            final String namespaceId) {
        this.proxyApiKey = proxyApiKey;
        this.enabled = enabled;
        this.pageParameter = pageParameter;
        this.namespaceId = namespaceId;
    }

    public String getProxyApiKey() {
        return proxyApiKey;
    }

    public void setProxyApiKey(final String proxyApiKey) {
        this.proxyApiKey = proxyApiKey;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    public PageParameter getPageParameter() {
        return pageParameter;
    }

    public void setPageParameter(final PageParameter pageParameter) {
        this.pageParameter = pageParameter;
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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ProxyApiKeyQuery)) {
            return false;
        }
        ProxyApiKeyQuery that = (ProxyApiKeyQuery) o;
        return Objects.equals(proxyApiKey, that.proxyApiKey)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(pageParameter, that.pageParameter)
                && Objects.equals(namespaceId, that.namespaceId)
                && Objects.equals(selectorId, that.selectorId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(proxyApiKey, enabled, pageParameter, namespaceId, selectorId);
    }
}