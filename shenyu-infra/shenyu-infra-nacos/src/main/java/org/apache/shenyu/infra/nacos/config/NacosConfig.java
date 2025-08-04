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

package org.apache.shenyu.infra.nacos.config;

import java.util.Objects;

public class NacosConfig {

    private String url;

    private String namespace;

    private String username;

    private String password;

    private String contextPath;

    private NacosACMConfig acm;

    public NacosConfig(final String url,
                       final String namespace,
                       final String username,
                       final String password,
                       final String contextPath,
                       final NacosACMConfig acm
    ) {

        this.url = url;
        this.namespace = namespace;
        this.username = username;
        this.password = password;
        this.contextPath = contextPath;
        this.acm = acm;
    }

    /**
     * get url.
     *
     * @return url
     */
    public String getUrl() {
        return url;
    }

    /**
     * set url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
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
     * get username.
     *
     * @return username
     */
    public String getUsername() {
        return username;
    }

    /**
     * set username.
     *
     * @param username username
     */
    public void setUsername(final String username) {
        this.username = username;
    }

    /**
     * get password.
     *
     * @return password
     */
    public String getPassword() {
        return password;
    }

    /**
     * set password.
     *
     * @param password password
     */
    public void setPassword(final String password) {
        this.password = password;
    }

    /**
     * Gets the value of contextPath.
     *
     * @return the value of contextPath
     */
    public String getContextPath() {
        return contextPath;
    }

    /**
     * Sets the contextPath.
     *
     * @param contextPath contextPath
     */
    public void setContextPath(final String contextPath) {
        this.contextPath = contextPath;
    }

    /**
     * get acm.
     *
     * @return acm
     */
    public NacosACMConfig getAcm() {
        return acm;
    }

    /**
     * set acm.
     *
     * @param acm acm
     */
    public void setAcm(final NacosACMConfig acm) {
        this.acm = acm;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        NacosConfig that = (NacosConfig) o;
        return Objects.equals(url, that.url)
                && Objects.equals(namespace, that.namespace)
                && Objects.equals(username, that.username)
                && Objects.equals(password, that.password)
                && Objects.equals(contextPath, that.contextPath)
                && Objects.equals(acm, that.acm);
    }

    @Override
    public int hashCode() {
        return Objects.hash(url, namespace, username, password, contextPath, acm);
    }

    @Override
    public String toString() {
        return "NacosConfig{"
                + "url='"
                + url
                + '\''
                + ", namespace='"
                + namespace
                + '\''
                + ", username='"
                + username
                + '\''
                + ", password='"
                + password
                + '\''
                + ", contextPath='"
                + contextPath
                + '\''
                + ", acm="
                + acm
                + '}';
    }

    public static Builder builder() {

        return new Builder();
    }

    public static final class Builder {

        private String url;

        private String namespace;

        private String username;

        private String password;

        private String contextPath;

        private NacosACMConfig acm;

        public Builder url(final String url) {
            this.url = url;
            return this;
        }

        public Builder namespace(final String namespace) {
            this.namespace = namespace;
            return this;
        }

        public Builder username(final String username) {
            this.username = username;
            return this;
        }

        public Builder password(final String password) {
            this.password = password;
            return this;
        }

        public Builder contextPath(final String contextPath) {
            this.contextPath = contextPath;
            return this;
        }

        public Builder acm(final NacosACMConfig acm) {
            this.acm = acm;
            return this;
        }

        public NacosConfig build() {
            return new NacosConfig(url, namespace, username, password, contextPath, acm);
        }

    }

}
