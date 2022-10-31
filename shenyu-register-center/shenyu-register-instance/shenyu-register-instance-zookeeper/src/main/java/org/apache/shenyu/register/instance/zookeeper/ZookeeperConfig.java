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

package org.apache.shenyu.register.instance.zookeeper;

public class ZookeeperConfig {
    
    /**
     * zookeeper server list.
     * e.g. host1:2181,host2:2181
     */
    private final String serverLists;

    /**
     * zookeeper namespace.
     */
    private String namespace = "";

    /**
     * initial amount of time to wait between retries.
     */
    private int baseSleepTimeMilliseconds = 1000;

    /**
     * max time in ms to sleep on each retry.
     */
    private int maxSleepTimeMilliseconds = Integer.MAX_VALUE;

    /**
     * max number of times to retry.
     */
    private int maxRetries = 3;

    /**
     * session timeout.
     */
    private int sessionTimeoutMilliseconds = 60 * 1000;

    /**
     * connection timeout.
     */
    private int connectionTimeoutMilliseconds = 15 * 1000;

    /**
     * auth token digest. no auth by default.
     */
    private String digest;

    public ZookeeperConfig(final String serverLists) {
        this.serverLists = serverLists;
    }

    /**
     * get zookeeper server list.
     * @return server list.
     */
    public String getServerLists() {
        return serverLists;
    }

    /**
     * set namespace.
     * @param namespace zk namespace
     * @return zk config
     */
    public ZookeeperConfig setNamespace(final String namespace) {
        this.namespace = namespace;
        return this;
    }

    /**
     * get namespace.
     * @return namespace
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * get base sleep time.
     * @return base sleep time.
     */
    public int getBaseSleepTimeMilliseconds() {
        return baseSleepTimeMilliseconds;
    }

    /**
     * set base sleep time.
     * @param baseSleepTimeMilliseconds base sleep time in milliseconds.
     * @return zk config.
     */
    public ZookeeperConfig setBaseSleepTimeMilliseconds(final int baseSleepTimeMilliseconds) {
        this.baseSleepTimeMilliseconds = baseSleepTimeMilliseconds;
        return this;
    }

    /**
     * get max sleep time.
     * @return max sleep time
     */
    public int getMaxSleepTimeMilliseconds() {
        return maxSleepTimeMilliseconds;
    }

    /**
     * set max sleep time.
     * @param maxSleepTimeMilliseconds max sleep time.
     * @return zk config.
     */
    public ZookeeperConfig setMaxSleepTimeMilliseconds(final int maxSleepTimeMilliseconds) {
        this.maxSleepTimeMilliseconds = maxSleepTimeMilliseconds;
        return this;
    }

    /**
     * get max retries.
     * @return max retries
     */
    public int getMaxRetries() {
        return maxRetries;
    }

    /**
     * set max retries count.
     * @param maxRetries max retries
     * @return zk config.
     */
    public ZookeeperConfig setMaxRetries(final int maxRetries) {
        this.maxRetries = maxRetries;
        return this;
    }

    /**
     * get session timeout in milliseconds.
     * @return session timeout.
     */
    public int getSessionTimeoutMilliseconds() {
        return sessionTimeoutMilliseconds;
    }

    /**
     * set session timeout in milliseconds.
     * @param sessionTimeoutMilliseconds session timeout
     * @return zk config.
     */
    public ZookeeperConfig setSessionTimeoutMilliseconds(final int sessionTimeoutMilliseconds) {
        this.sessionTimeoutMilliseconds = sessionTimeoutMilliseconds;
        return this;
    }

    /**
     * get connection timeout in milliseconds.
     * @return connection timeout.
     */
    public int getConnectionTimeoutMilliseconds() {
        return connectionTimeoutMilliseconds;
    }

    /**
     * set connection timeout in milliseconds.
     * @param connectionTimeoutMilliseconds connection timeout.
     * @return zk config.
     */
    public ZookeeperConfig setConnectionTimeoutMilliseconds(final int connectionTimeoutMilliseconds) {
        this.connectionTimeoutMilliseconds = connectionTimeoutMilliseconds;
        return this;
    }

    /**
     * get digest.
     * @return digest.
     */
    public String getDigest() {
        return digest;
    }

    /**
     * set digest.
     * @param digest digest
     * @return zk config.
     */
    public ZookeeperConfig setDigest(final String digest) {
        this.digest = digest;
        return this;
    }
}
