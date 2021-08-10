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

public class NacosConfig {

    private String url;

    private String namespace;

    private String username;

    private String password;

    private NacosACMConfig acm;

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
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        NacosConfig that = (NacosConfig) o;
        return Objects.equals(url, that.url)
                && Objects.equals(namespace, that.namespace)
                && Objects.equals(username, that.username)
                && Objects.equals(password, that.password)
                && Objects.equals(acm, that.acm);
    }

    @Override
    public int hashCode() {
        return Objects.hash(url, namespace, username, password, acm);
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
                + ", acm="
                + acm
                + '}';
    }
}
