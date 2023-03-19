package org.apache.shenyu.sync.data.apollo.config;/*
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


public class ApolloConfig {

    /**
     * appId.
     */
    private String appId;

    /**
     * apollo config service url.
     */
    private String meta;

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


    public String getAppId() {
        return appId;
    }

    public void setAppId(String appId) {
        this.appId = appId;
    }

    public String getMeta() {
        return meta;
    }

    public void setMeta(String meta) {
        this.meta = meta;
    }

    public String getPortalUrl() {
        return portalUrl;
    }

    public void setPortalUrl(String portalUrl) {
        this.portalUrl = portalUrl;
    }

    public String getEnv() {
        return env;
    }

    public void setEnv(String env) {
        this.env = env;
    }

    public String getClusterName() {
        return clusterName;
    }

    public void setClusterName(String clusterName) {
        this.clusterName = clusterName;
    }

    public String getNamespace() {
        return namespace;
    }

    public void setNamespace(String namespace) {
        this.namespace = namespace;
    }

    public ApolloConfig() {
    }

    public ApolloConfig(String appId, String meta, String portalUrl, String env, String clusterName, String namespace) {
        this.appId = appId;
        this.meta = meta;
        this.portalUrl = portalUrl;
        this.env = env;
        this.clusterName = clusterName;
        this.namespace = namespace;
    }
}