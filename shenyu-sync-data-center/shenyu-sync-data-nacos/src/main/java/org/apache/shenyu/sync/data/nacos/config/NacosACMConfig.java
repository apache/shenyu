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

package org.apache.shenyu.sync.data.nacos.config;

import java.util.Objects;

public class NacosACMConfig {

    private boolean enabled;

    private String endpoint;

    private String namespace;

    private String accessKey;

    private String secretKey;

    /**
     * get enabled.
     *
     * @return enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * set enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * get endpoint.
     *
     * @return endpoint
     */
    public String getEndpoint() {
        return endpoint;
    }

    /**
     * set endpoint.
     *
     * @param endpoint endpoint
     */
    public void setEndpoint(final String endpoint) {
        this.endpoint = endpoint;
    }

    /**
     * get namespace.
     *
     * @return namespace
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * set namespace.
     *
     * @param namespace namespace
     */
    public void setNamespace(final String namespace) {
        this.namespace = namespace;
    }

    /**
     * get accessKey.
     *
     * @return accessKey
     */
    public String getAccessKey() {
        return accessKey;
    }

    /**
     * set accessKey.
     *
     * @param accessKey accessKey
     */
    public void setAccessKey(final String accessKey) {
        this.accessKey = accessKey;
    }

    /**
     * get secretKey.
     *
     * @return secretKey
     */
    public String getSecretKey() {
        return secretKey;
    }

    /**
     * set secretKey.
     *
     * @param secretKey secretKey
     */
    public void setSecretKey(final String secretKey) {
        this.secretKey = secretKey;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        NacosACMConfig that = (NacosACMConfig) o;
        return enabled == that.enabled
                && Objects.equals(endpoint, that.endpoint)
                && Objects.equals(namespace, that.namespace)
                && Objects.equals(accessKey, that.accessKey)
                && Objects.equals(secretKey, that.secretKey);
    }

    @Override
    public int hashCode() {
        return Objects.hash(enabled, endpoint, namespace, accessKey, secretKey);
    }

    @Override
    public String toString() {
        return "NacosACMConfig{"
                + "enabled="
                + enabled
                + ", endpoint='"
                + endpoint
                + '\''
                + ", namespace='"
                + namespace
                + '\''
                + ", accessKey='"
                + accessKey
                + '\''
                + ", secretKey='"
                + secretKey
                + '\''
                + '}';
    }
}
