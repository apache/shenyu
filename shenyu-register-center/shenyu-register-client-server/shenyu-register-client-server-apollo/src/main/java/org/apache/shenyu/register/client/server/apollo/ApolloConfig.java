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

package org.apache.shenyu.register.client.server.apollo;

public class ApolloConfig {

    /**
     * appId.
     */
    private String appId;

    /**
     * portal url.
     * e.g. localhost:8080
     */
    private String portalUrl;

    /**
     * env.
     * e.g. ENV
     */
    private String env;

    /**
     * cluster name.
     */
    private String clusterName;

    /**
     * namespace name.
     */
    private String namespace;

    /**
     * open api use token.
     */
    private String token;

    /**
     * get appId.
     * @return appId
     */
    public String getAppId() {
        return appId;
    }

    /**
     * set app id.
     * @param appId app id
     */
    public void setAppId(final String appId) {
        this.appId = appId;
    }

    /**
     * get portal url.
     * @return portal url
     */
    public String getPortalUrl() {
        return portalUrl;
    }

    /**
     * set portal url.
     * @param portalUrl portal url
     */
    public void setPortalUrl(final String portalUrl) {
        this.portalUrl = portalUrl;
    }

    /**
     * get env.
     * @return env
     */
    public String getEnv() {
        return env;
    }

    /**
     * set env.
     * @param env env
     */
    public void setEnv(final String env) {
        this.env = env;
    }

    /**
     * get cluster name.
     * @return cluster name
     */
    public String getClusterName() {
        return clusterName;
    }

    /**
     * set cluster name.
     * @param clusterName cluster name
     */
    public void setClusterName(final String clusterName) {
        this.clusterName = clusterName;
    }

    /**
     * get namespace name.
     * @return namespace name
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * set namespace name.
     * @param namespace namespace name
     */
    public void setNamespace(final String namespace) {
        this.namespace = namespace;
    }

    /**
     * get token.
     * @return token
     */
    public String getToken() {
        return token;
    }

    /**
     * set token.
     * @param token token
     */
    public void setToken(final String token) {
        this.token = token;
    }
}
