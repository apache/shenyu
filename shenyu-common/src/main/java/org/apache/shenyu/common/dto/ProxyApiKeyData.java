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

/** ProxyApiKeyData for syncing proxy->real API key mappings to gateway. */
public class ProxyApiKeyData {

    private String realApiKey;

    private String proxyApiKey;

    private String description;

    private Boolean enabled;

    private String namespaceId;

    /**
     * Selector id to which this mapping belongs.
     */
    private String selectorId;

    public ProxyApiKeyData() {
    }

    private ProxyApiKeyData(final Builder builder) {
        this.realApiKey = builder.realApiKey;
        this.proxyApiKey = builder.proxyApiKey;
        this.description = builder.description;
        this.enabled = builder.enabled;
        this.namespaceId = builder.namespaceId;
        this.selectorId = builder.selectorId;
    }

    public static Builder builder() {
        return new Builder();
    }

    public String getRealApiKey() {
        return realApiKey;
    }

    public void setRealApiKey(final String realApiKey) {
        this.realApiKey = realApiKey;
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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        ProxyApiKeyData that = (ProxyApiKeyData) o;
        return Objects.equals(realApiKey, that.realApiKey)
                && Objects.equals(proxyApiKey, that.proxyApiKey)
                && Objects.equals(description, that.description)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(namespaceId, that.namespaceId)
                && Objects.equals(selectorId, that.selectorId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(realApiKey, proxyApiKey, description, enabled, namespaceId, selectorId);
    }

    public static final class Builder {
        private String realApiKey;

        private String proxyApiKey;

        private String description;

        private Boolean enabled;

        private String namespaceId;

        private String selectorId;

        private Builder() {
        }

        public Builder realApiKey(final String realApiKey) {
            this.realApiKey = realApiKey;
            return this;
        }

        public Builder proxyApiKey(final String proxyApiKey) {
            this.proxyApiKey = proxyApiKey;
            return this;
        }

        public Builder description(final String description) {
            this.description = description;
            return this;
        }

        public Builder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        public Builder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        public Builder selectorId(final String selectorId) {
            this.selectorId = selectorId;
            return this;
        }

        public ProxyApiKeyData build() {
            return new ProxyApiKeyData(this);
        }
    }
}
