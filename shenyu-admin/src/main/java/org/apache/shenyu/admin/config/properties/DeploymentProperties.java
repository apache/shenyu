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

@ConfigurationProperties(prefix = "shenyu.k8s.deployment")
public class DeploymentProperties {

    private String name;

    private String namespace;

    private String apiServer;

    private String token;

    private String caCertPath;

    /**
     * getName.
     *
     * @return String
     */
    public String getName() {
        return name;
    }

    /**
     * setName.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * getNamespace.
     *
     * @return String
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * setNamespace.
     *
     * @param namespace namespace
     */
    public void setNamespace(final String namespace) {
        this.namespace = namespace;
    }

    /**
     * getApiServer.
     *
     * @return String
     */
    public String getApiServer() {
        return apiServer;
    }

    /**
     * setApiServer.
     *
     * @param apiServer apiServer
     */
    public void setApiServer(final String apiServer) {
        this.apiServer = apiServer;
    }

    /**
     * getToken.
     *
     * @return String
     */
    public String getToken() {
        return token;
    }

    /**
     * setToken.
     *
     * @param token token
     */
    public void setToken(final String token) {
        this.token = token;
    }

    /**
     * getCaCertPath.
     *
     * @return String
     */
    public String getCaCertPath() {
        return caCertPath;
    }

    /**
     * setCaCertPath.
     *
     * @param caCertPath caCertPath
     */
    public void setCaCertPath(final String caCertPath) {
        this.caCertPath = caCertPath;
    }

}
