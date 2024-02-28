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

package org.apache.shenyu.sync.data.apollo.config;

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
     * accessKey.
     */
    private String accessKey;

    /**
     * constructor.
     */
    public ApolloConfig() {
    }

    /**
     * getSecretKey.
     * @return secretKey
     */
    public String getAccessKey() {
        return accessKey;
    }

    /**
     * set secretKey.
     * @param accessKey accessKey
     */
    public void setAccessKey(final String accessKey) {
        this.accessKey = accessKey;
    }

    /**
     * get appId.
     * @return appId
     */
    public String getAppId() {
        return appId;
    }

    /**
     * set appId.
     * @param appId appId
     */
    public void setAppId(final String appId) {
        this.appId = appId;
    }

    /**
     * get meta.
     * @return meta
     */
    public String getMeta() {
        return meta;
    }

    /**
     * set meta.
     * @param meta meta
     */
    public void setMeta(final String meta) {
        this.meta = meta;
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
     * get clusterName.
     * @return clusterName
     */
    public String getClusterName() {
        return clusterName;
    }

    /**
     * set clusterName.
     * @param clusterName clusterName
     */
    public void setClusterName(final String clusterName) {
        this.clusterName = clusterName;
    }

    /**
     * get namespace.
     * @return namespace
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * set namespace.
     * @param namespace namespace
     */
    public void setNamespace(final String namespace) {
        this.namespace = namespace;
    }

}
