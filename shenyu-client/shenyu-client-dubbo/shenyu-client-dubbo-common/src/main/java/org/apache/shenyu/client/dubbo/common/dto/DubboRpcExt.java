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

package org.apache.shenyu.client.dubbo.common.dto;

import java.io.Serializable;

/**
 * The type Dubbo rpc ext.
 */
public class DubboRpcExt implements Serializable {

    private static final long serialVersionUID = -1771273152608873279L;

    private String group;

    private String version;

    private String loadbalance;

    private Integer retries;

    private Integer timeout;

    private String url;

    /**
     * constructor without parameter.
     */
    public DubboRpcExt() {
    }

    /**
     * constructor with all parameters.
     *
     * @param group group
     * @param version version
     * @param loadbalance loadbalance
     * @param retries retries
     * @param timeout timeout
     * @param url url
     */
    public DubboRpcExt(final String group, final String version, final String loadbalance, final Integer retries, final Integer timeout, final String url) {
        this.group = group;
        this.version = version;
        this.loadbalance = loadbalance;
        this.retries = retries;
        this.timeout = timeout;
        this.url = url;
    }

    /**
     * get group.
     *
     * @return group
     */
    public String getGroup() {
        return group;
    }

    /**
     * set group.
     *
     * @param group group
     */
    public void setGroup(final String group) {
        this.group = group;
    }

    /**
     * get version.
     *
     * @return version
     */
    public String getVersion() {
        return version;
    }

    /**
     * set version.
     *
     * @param version version
     */
    public void setVersion(final String version) {
        this.version = version;
    }

    /**
     * get loadbalance.
     *
     * @return loadbalance
     */
    public String getLoadbalance() {
        return loadbalance;
    }

    /**
     * set loadbalance.
     *
     * @param loadbalance loadbalance
     */
    public void setLoadbalance(final String loadbalance) {
        this.loadbalance = loadbalance;
    }

    /**
     * get retries.
     *
     * @return retries
     */
    public Integer getRetries() {
        return retries;
    }

    /**
     * set retries.
     *
     * @param retries retries
     */
    public void setRetries(final Integer retries) {
        this.retries = retries;
    }

    /**
     * get timeout.
     *
     * @return timeout
     */
    public Integer getTimeout() {
        return timeout;
    }

    /**
     * set timeout.
     *
     * @param timeout timeout
     */
    public void setTimeout(final Integer timeout) {
        this.timeout = timeout;
    }

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

    @Override
    public String toString() {
        return "DubboRpcExt{"
                + "group='" + group + '\''
                + ", version='" + version + '\''
                + ", loadbalance='" + loadbalance + '\''
                + ", retries=" + retries
                + ", timeout=" + timeout
                + ", url='" + url + '\''
                + '}';
    }

    /**
     * get Builder of DubboRpcExt.
     *
     * @return the Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * the Builder of DubboRpcExt.
     */
    public static final class Builder {

        private String group;

        private String version;

        private String loadbalance;

        private Integer retries;

        private Integer timeout;

        private String url;

        /**
         * constructor without parameter.
         */
        private Builder() {
        }

        /**
         * set group.
         *
         * @param group group
         * @return Builder
         */
        public Builder group(final String group) {
            this.group = group;
            return this;
        }

        /**
         * set version.
         *
         * @param version version
         * @return Builder
         */
        public Builder version(final String version) {
            this.version = version;
            return this;
        }

        /**
         * set loadbalance.
         *
         * @param loadbalance loadbalance
         * @return Builder
         */
        public Builder loadbalance(final String loadbalance) {
            this.loadbalance = loadbalance;
            return this;
        }

        /**
         * set retries.
         *
         * @param retries retries
         * @return Builder
         */
        public Builder retries(final Integer retries) {
            this.retries = retries;
            return this;
        }

        /**
         * set timeout.
         *
         * @param timeout timeout
         * @return Builder
         */
        public Builder timeout(final Integer timeout) {
            this.timeout = timeout;
            return this;
        }

        /**
         * set url.
         *
         * @param url url
         * @return Builder
         */
        public Builder url(final String url) {
            this.url = url;
            return this;
        }

        /**
         * build DubboRpcExt.
         *
         * @return DubboRpcExt
         */
        public DubboRpcExt build() {
            DubboRpcExt dubboRpcExt = new DubboRpcExt();
            dubboRpcExt.setGroup(group);
            dubboRpcExt.setVersion(version);
            dubboRpcExt.setLoadbalance(loadbalance);
            dubboRpcExt.setRetries(retries);
            dubboRpcExt.setTimeout(timeout);
            dubboRpcExt.setUrl(url);
            return dubboRpcExt;
        }
    }
}
