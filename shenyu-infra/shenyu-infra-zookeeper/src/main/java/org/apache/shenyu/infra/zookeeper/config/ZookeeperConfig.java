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

package org.apache.shenyu.infra.zookeeper.config;

import java.util.Objects;

/**
 * The type zookeeper configuration.
 */
public class ZookeeperConfig {

    private String url;

    private String namespace;

    private Integer baseSleepTimeMilliseconds;

    private Integer maxSleepTimeMilliseconds;

    private Integer maxRetries;

    private Integer sessionTimeoutMilliseconds;

    private Integer connectionTimeoutMilliseconds;

    private String digest;

    public ZookeeperConfig(final String url,
                           final String namespace,
                           final Integer baseSleepTimeMilliseconds,
                           final Integer maxSleepTimeMilliseconds,
                           final Integer maxRetries,
                           final Integer sessionTimeoutMilliseconds,
                           final Integer connectionTimeoutMilliseconds,
                           final String digest) {
        this.url = url;
        this.namespace = namespace;
        this.baseSleepTimeMilliseconds = baseSleepTimeMilliseconds;
        this.maxSleepTimeMilliseconds = maxSleepTimeMilliseconds;
        this.maxRetries = maxRetries;
        this.sessionTimeoutMilliseconds = sessionTimeoutMilliseconds;
        this.connectionTimeoutMilliseconds = connectionTimeoutMilliseconds;
        this.digest = digest;
    }

    /**
     * Get url.
     *
     * @return url
     */
    public String getUrl() {
        return url;
    }

    /**
     * Set url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * Get namespace.
     *
     * @return namespace
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * Set namespace.
     *
     * @param namespace namespace
     */
    public void setNamespace(final String namespace) {
        this.namespace = namespace;
    }

    /**
     * Get baseSleepTimeMilliseconds.
     *
     * @return baseSleepTimeMilliseconds
     */
    public Integer getBaseSleepTimeMilliseconds() {
        return baseSleepTimeMilliseconds;
    }

    /**
     * Set baseSleepTimeMilliseconds.
     *
     * @param baseSleepTimeMilliseconds baseSleepTimeMilliseconds
     */
    public void setBaseSleepTimeMilliseconds(final Integer baseSleepTimeMilliseconds) {
        this.baseSleepTimeMilliseconds = baseSleepTimeMilliseconds;
    }

    /**
     * Get maxSleepTimeMilliseconds.
     *
     * @return maxSleepTimeMilliseconds
     */
    public Integer getMaxSleepTimeMilliseconds() {
        return maxSleepTimeMilliseconds;
    }

    /**
     * Set maxSleepTimeMilliseconds.
     *
     * @param maxSleepTimeMilliseconds maxSleepTimeMilliseconds
     */
    public void setMaxSleepTimeMilliseconds(final Integer maxSleepTimeMilliseconds) {
        this.maxSleepTimeMilliseconds = maxSleepTimeMilliseconds;
    }

    /**
     * Get maxRetries.
     *
     * @return maxRetries
     */
    public Integer getMaxRetries() {
        return maxRetries;
    }

    /**
     * Set maxRetries.
     *
     * @param maxRetries maxRetries
     */
    public void setMaxRetries(final Integer maxRetries) {
        this.maxRetries = maxRetries;
    }

    /**
     * Get sessionTimeoutMilliseconds.
     *
     * @return sessionTimeoutMilliseconds
     */
    public Integer getSessionTimeoutMilliseconds() {
        return sessionTimeoutMilliseconds;
    }

    /**
     * Set sessionTimeoutMilliseconds.
     *
     * @param sessionTimeoutMilliseconds sessionTimeoutMilliseconds
     */
    public void setSessionTimeoutMilliseconds(final Integer sessionTimeoutMilliseconds) {
        this.sessionTimeoutMilliseconds = sessionTimeoutMilliseconds;
    }

    /**
     * Get connectionTimeoutMilliseconds.
     *
     * @return connectionTimeoutMilliseconds
     */
    public Integer getConnectionTimeoutMilliseconds() {
        return connectionTimeoutMilliseconds;
    }

    /**
     * Set connectionTimeoutMilliseconds.
     *
     * @param connectionTimeoutMilliseconds connectionTimeoutMilliseconds
     */
    public void setConnectionTimeoutMilliseconds(final Integer connectionTimeoutMilliseconds) {
        this.connectionTimeoutMilliseconds = connectionTimeoutMilliseconds;
    }

    /**
     * Get digest.
     *
     * @return digest
     */
    public String getDigest() {
        return digest;
    }

    /**
     * Set digest.
     *
     * @param digest digest
     */
    public void setDigest(final String digest) {
        this.digest = digest;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        ZookeeperConfig that = (ZookeeperConfig) o;
        return Objects.equals(url, that.url)
                && Objects.equals(namespace, that.namespace)
                && Objects.equals(baseSleepTimeMilliseconds, that.baseSleepTimeMilliseconds)
                && Objects.equals(maxSleepTimeMilliseconds, that.maxSleepTimeMilliseconds)
                && Objects.equals(maxRetries, that.maxRetries)
                && Objects.equals(sessionTimeoutMilliseconds, that.sessionTimeoutMilliseconds)
                && Objects.equals(connectionTimeoutMilliseconds, that.connectionTimeoutMilliseconds)
                && Objects.equals(digest, that.digest);
    }

    @Override
    public int hashCode() {
        return Objects.hash(url, namespace, baseSleepTimeMilliseconds, maxSleepTimeMilliseconds,
                maxRetries, sessionTimeoutMilliseconds, connectionTimeoutMilliseconds, digest);
    }

    @Override
    public String toString() {
        return "ZookeeperConfig{"
                + "url='" + url + '\''
                + ", namespace='" + namespace + '\''
                + ", baseSleepTimeMilliseconds=" + baseSleepTimeMilliseconds
                + ", maxSleepTimeMilliseconds=" + maxSleepTimeMilliseconds
                + ", maxRetries=" + maxRetries
                + ", sessionTimeoutMilliseconds=" + sessionTimeoutMilliseconds
                + ", connectionTimeoutMilliseconds=" + connectionTimeoutMilliseconds
                + ", digest='" + digest + '\''
                + '}';
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder {

        private String url;

        private String namespace;

        private Integer baseSleepTimeMilliseconds;

        private Integer maxSleepTimeMilliseconds;

        private Integer maxRetries;

        private Integer sessionTimeoutMilliseconds;

        private Integer connectionTimeoutMilliseconds;

        private String digest;

        /**
         * private constructor. not allow to create instance.
         */
        private Builder() {
        }

        public Builder url(final String url) {
            this.url = url;
            return this;
        }

        public Builder namespace(final String namespace) {
            this.namespace = namespace;
            return this;
        }

        public Builder baseSleepTimeMilliseconds(final Integer baseSleepTimeMilliseconds) {
            this.baseSleepTimeMilliseconds = baseSleepTimeMilliseconds;
            return this;
        }

        public Builder maxSleepTimeMilliseconds(final Integer maxSleepTimeMilliseconds) {
            this.maxSleepTimeMilliseconds = maxSleepTimeMilliseconds;
            return this;
        }

        public Builder maxRetries(final Integer maxRetries) {
            this.maxRetries = maxRetries;
            return this;
        }

        public Builder sessionTimeoutMilliseconds(final Integer sessionTimeoutMilliseconds) {
            this.sessionTimeoutMilliseconds = sessionTimeoutMilliseconds;
            return this;
        }

        public Builder connectionTimeoutMilliseconds(final Integer connectionTimeoutMilliseconds) {
            this.connectionTimeoutMilliseconds = connectionTimeoutMilliseconds;
            return this;
        }

        public Builder digest(final String digest) {
            this.digest = digest;
            return this;
        }

        public ZookeeperConfig build() {
            return new ZookeeperConfig(url, namespace, baseSleepTimeMilliseconds,
                    maxSleepTimeMilliseconds, maxRetries, sessionTimeoutMilliseconds,
                    connectionTimeoutMilliseconds, digest);
        }
    }
}
