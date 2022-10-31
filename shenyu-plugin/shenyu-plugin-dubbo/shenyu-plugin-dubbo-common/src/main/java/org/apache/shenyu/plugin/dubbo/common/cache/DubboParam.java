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

package org.apache.shenyu.plugin.dubbo.common.cache;

/**
 * DubboParam.
 */
public class DubboParam {

    /**
     * the group.
     */
    private String group;

    /**
     * the version.
     */
    private String version;

    /**
     * the loadbalance.
     */
    private String loadbalance;

    /**
     * the retries.
     */
    private Integer retries;

    /**
     * the timeout.
     */
    private Integer timeout;

    /**
     * the url.
     */
    private String url;

    /**
     * the sent.
     */
    private Boolean sent;

    /**
     * the cluster.
     */
    private String cluster;

    /**
     * Gets group.
     *
     * @return the group
     */
    public String getGroup() {
        return group;
    }

    /**
     * Sets group.
     *
     * @param group the group
     */
    public void setGroup(final String group) {
        this.group = group;
    }

    /**
     * Gets version.
     *
     * @return the version
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets version.
     *
     * @param version the version
     */
    public void setVersion(final String version) {
        this.version = version;
    }

    /**
     * Gets loadbalance.
     *
     * @return the loadbalance
     */
    public String getLoadbalance() {
        return loadbalance;
    }

    /**
     * Sets loadbalance.
     *
     * @param loadbalance the loadbalance
     */
    public void setLoadbalance(final String loadbalance) {
        this.loadbalance = loadbalance;
    }

    /**
     * Gets retries.
     *
     * @return the retries
     */
    public Integer getRetries() {
        return retries;
    }

    /**
     * Sets retries.
     *
     * @param retries the retries
     */
    public void setRetries(final Integer retries) {
        this.retries = retries;
    }

    /**
     * Gets timeout.
     *
     * @return the timeout
     */
    public Integer getTimeout() {
        return timeout;
    }

    /**
     * Sets timeout.
     *
     * @param timeout the timeout
     */
    public void setTimeout(final Integer timeout) {
        this.timeout = timeout;
    }

    /**
     * Gets url.
     *
     * @return the url
     */
    public String getUrl() {
        return url;
    }

    /**
     * Sets url.
     *
     * @param url the url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * Gets sent.
     *
     * @return the sent
     */
    public Boolean getSent() {
        return sent;
    }

    /**
     * Sets sent.
     *
     * @param sent the sent
     */
    public void setSent(final Boolean sent) {
        this.sent = sent;
    }

    /**
     * Gets cluster.
     *
     * @return the cluster
     */
    public String getCluster() {
        return cluster;
    }

    /**
     * Sets cluster.
     *
     * @param cluster the cluster
     */
    public void setCluster(final String cluster) {
        this.cluster = cluster;
    }
}
