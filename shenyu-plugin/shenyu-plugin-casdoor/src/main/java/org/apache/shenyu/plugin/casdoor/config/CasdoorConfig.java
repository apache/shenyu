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

package org.apache.shenyu.plugin.casdoor.config;

public class CasdoorConfig {

    private String endpoint;

    private String clientId;

    private String clientSecret;

    private String certificate;

    private String organizationName;

    private String applicationName;

    public CasdoorConfig(final String endpoint, final String clientId, final String clientSecret, final String certificate, final String organizationName, final String applicationName) {
        this.endpoint = endpoint;
        this.clientId = clientId;
        this.clientSecret = clientSecret;
        this.certificate = certificate;
        this.organizationName = organizationName;
        this.applicationName = applicationName;
    }

    public CasdoorConfig() {
    }

    /**
     * Gets endpoint.
     *
     * @return the endpoint
     */
    public String getEndpoint() {
        return this.endpoint;
    }

    /**
     * Gets clientId.
     *
     * @return the clientId
     */
    public String getClientId() {
        return this.clientId;
    }

    /**
     * Gets clientSecret.
     *
     * @return the clientSecret
     */
    public String getClientSecret() {
        return this.clientSecret;
    }

    /**
     * Gets certificate.
     *
     * @return the certificate
     */
    public String getCertificate() {
        return this.certificate;
    }

    /**
     * Gets organizationName.
     *
     * @return the organizationName
     */
    public String getOrganizationName() {
        return this.organizationName;
    }

    /**
     * Gets applicationName.
     *
     * @return the applicationName
     */
    public String getApplicationName() {
        return this.applicationName;
    }

    /**
     * Sets endpoint.
     *
     * @param endpoint the endpoint
     */
    public void setEndpoint(final String endpoint) {
        this.endpoint = endpoint;
    }

    /**
     * Sets clientId.
     *
     * @param clientId the clientId
     */
    public void setClientId(final String clientId) {
        this.clientId = clientId;
    }

    /**
     * Sets clientSecret.
     *
     * @param clientSecret the clientSecret
     */
    public void setClientSecret(final String clientSecret) {
        this.clientSecret = clientSecret;
    }

    /**
     * Sets certificate.
     *
     * @param certificate the certificate
     */
    public void setCertificate(final String certificate) {
        this.certificate = certificate;
    }

    /**
     * Sets organizationName.
     *
     * @param organizationName the organizationName
     */
    public void setOrganizationName(final String organizationName) {
        this.organizationName = organizationName;
    }

    /**
     * Sets applicationName.
     *
     * @param applicationName the applicationName
     */
    public void setApplicationName(final String applicationName) {
        this.applicationName = applicationName;
    }

    @Override
    public String toString() {
        return "authConfig{"
                + "endpoint='" + endpoint + '\''
                + ", clientId='" + clientId + '\''
                + ", clientSecret='" + clientSecret + '\''
                + ", certificate='" + certificate + '\''
                + ", organizationName='" + organizationName + '\''
                + ", applicationName='" + applicationName + '\''
                + '}';
    }
}
