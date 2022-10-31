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

package org.apache.shenyu.admin.config.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Ldap properties.
 */
@ConfigurationProperties(prefix = "shenyu.ldap")
public class LdapProperties {

    /**
     * default: true.
     */
    private boolean enabled = true;

    private String url;

    private String bindDn;

    private String password;

    private String baseDn;

    private String objectClass = "person";

    private String loginField = "cn";

    private Integer connectTimeout = 3000;

    private Integer readTimeout = 3000;

    /**
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Gets the value of url.
     *
     * @return the value of url
     */
    public String getUrl() {
        return url;
    }

    /**
     * Sets the url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * Gets the value of bindDn.
     *
     * @return the value of bindDn
     */
    public String getBindDn() {
        return bindDn;
    }

    /**
     * Sets the bindDn.
     *
     * @param bindDn bindDn
     */
    public void setBindDn(final String bindDn) {
        this.bindDn = bindDn;
    }

    /**
     * Gets the value of password.
     *
     * @return the value of password
     */
    public String getPassword() {
        return password;
    }

    /**
     * Sets the password.
     *
     * @param password password
     */
    public void setPassword(final String password) {
        this.password = password;
    }

    /**
     * Gets the value of baseDn.
     *
     * @return the value of baseDn
     */
    public String getBaseDn() {
        return baseDn;
    }

    /**
     * Sets the baseDn.
     *
     * @param baseDn baseDn
     */
    public void setBaseDn(final String baseDn) {
        this.baseDn = baseDn;
    }

    /**
     * Gets the value of objectClass.
     *
     * @return the value of objectClass
     */
    public String getObjectClass() {
        return objectClass;
    }

    /**
     * Sets the objectClass.
     *
     * @param objectClass objectClass
     */
    public void setObjectClass(final String objectClass) {
        this.objectClass = objectClass;
    }

    /**
     * Gets the value of loginField.
     *
     * @return the value of loginField
     */
    public String getLoginField() {
        return loginField;
    }

    /**
     * Sets the loginField.
     *
     * @param loginField loginField
     */
    public void setLoginField(final String loginField) {
        this.loginField = loginField;
    }

    /**
     * Gets the value of connectTimeout.
     *
     * @return the value of connectTimeout
     */
    public Integer getConnectTimeout() {
        return connectTimeout;
    }

    /**
     * Sets the connectTimeout.
     *
     * @param connectTimeout connectTimeout
     */
    public void setConnectTimeout(final Integer connectTimeout) {
        this.connectTimeout = connectTimeout;
    }

    /**
     * Gets the value of readTimeout.
     *
     * @return the value of readTimeout
     */
    public Integer getReadTimeout() {
        return readTimeout;
    }

    /**
     * Sets the readTimeout.
     *
     * @param readTimeout readTimeout
     */
    public void setReadTimeout(final Integer readTimeout) {
        this.readTimeout = readTimeout;
    }
}
