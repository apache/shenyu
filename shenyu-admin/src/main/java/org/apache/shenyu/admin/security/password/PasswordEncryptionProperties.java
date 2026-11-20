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

package org.apache.shenyu.admin.security.password;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * Password request encryption settings. Private key material must remain outside source control.
 */
@Component
@ConfigurationProperties(prefix = "shenyu.password-encryption")
public class PasswordEncryptionProperties {

    private boolean required;

    private String privateKey;

    private int maxAgeSeconds = 120;

    /**
     * Whether legacy password requests are rejected.
     * @return required flag
     */
    public boolean isRequired() {
        return required;
    }

    /**
     * Set the legacy compatibility policy.
     * @param required required flag
     */
    public void setRequired(final boolean required) {
        this.required = required;
    }

    /**
     * Get the Base64 PKCS8 or PEM private key.
     * @return private key configuration
     */
    public String getPrivateKey() {
        return privateKey;
    }

    /**
     * Configure a private key shared by all Admin instances.
     * @param privateKey PKCS8 private key
     */
    public void setPrivateKey(final String privateKey) {
        this.privateKey = privateKey;
    }

    /**
     * Get the accepted request age.
     * @return maximum age in seconds
     */
    public int getMaxAgeSeconds() {
        return maxAgeSeconds;
    }

    /**
     * Set the accepted request age.
     * @param maxAgeSeconds maximum age in seconds
     */
    public void setMaxAgeSeconds(final int maxAgeSeconds) {
        this.maxAgeSeconds = maxAgeSeconds;
    }
}
