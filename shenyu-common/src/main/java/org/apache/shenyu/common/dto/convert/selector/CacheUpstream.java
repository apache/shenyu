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

package org.apache.shenyu.common.dto.convert.selector;

import java.util.Objects;

/**
 * The type Cache selector upstream.
 */
public class CacheUpstream extends CommonUpstream {

    /**
     * cacheType.
     */
    private String cacheType;

    /**
     * url.
     */
    private String url;

    /**
     * password.
     */
    private String password;

    /**
     * database.
     */
    private String database;

    /**
     * master.
     */
    private String master;

    /**
     * mode.
     */
    private String mode;

    /**
     * maxIdle.
     */
    private int maxIdle;

    /**
     * minIdle.
     */
    private int minIdle;

    /**
     * maxActive.
     */
    private int maxActive;

    /**
     * maxWait.
     */
    private int maxWait;

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    public CacheUpstream(final Builder builder) {
    }

    /**
     * class builder.
     *
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * get cacheType.
     *
     * @return cacheType
     */
    public String getCacheType() {
        return cacheType;
    }

    /**
     * set cacheType.
     *
     * @param cacheType cacheType
     */
    public void setCacheType(final String cacheType) {
        this.cacheType = cacheType;
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

    /**
     * get password.
     *
     * @return password
     */
    public String getPassword() {
        return password;
    }

    /**
     * set password.
     *
     * @param password password
     */
    public void setPassword(final String password) {
        this.password = password;
    }

    /**
     * get database.
     *
     * @return database
     */
    public String getDatabase() {
        return database;
    }

    /**
     * set database.
     *
     * @param database database
     */
    public void setDatabase(final String database) {
        this.database = database;
    }

    /**
     * get master.
     *
     * @return master
     */
    public String getMaster() {
        return master;
    }

    /**
     * set master.
     *
     * @param master master
     */
    public void setMaster(final String master) {
        this.master = master;
    }

    /**
     * get mode.
     *
     * @return mode
     */
    public String getMode() {
        return mode;
    }

    /**
     * set mode.
     *
     * @param mode mode
     */
    public void setMode(final String mode) {
        this.mode = mode;
    }

    /**
     * get maxIdle.
     *
     * @return maxIdle
     */
    public int getMaxIdle() {
        return maxIdle;
    }

    /**
     * set maxIdle.
     *
     * @param maxIdle maxIdle
     */
    public void setMaxIdle(final int maxIdle) {
        this.maxIdle = maxIdle;
    }

    /**
     * get minIdle.
     *
     * @return minIdle
     */
    public int getMinIdle() {
        return minIdle;
    }

    /**
     * set minIdle.
     *
     * @param minIdle minIdle
     */
    public void setMinIdle(final int minIdle) {
        this.minIdle = minIdle;
    }

    /**
     * get maxActive.
     *
     * @return maxActive
     */
    public int getMaxActive() {
        return maxActive;
    }

    /**
     * set maxActive.
     *
     * @param maxActive maxActive
     */
    public void setMaxActive(final int maxActive) {
        this.maxActive = maxActive;
    }

    /**
     * get maxWait.
     *
     * @return maxWait
     */
    public int getMaxWait() {
        return maxWait;
    }

    /**
     * set maxWait.
     *
     * @param maxWait maxWait
     */
    public void setMaxWait(final int maxWait) {
        this.maxWait = maxWait;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof CacheUpstream)) {
            return false;
        }
        CacheUpstream that = (CacheUpstream) o;
        return Objects.equals(cacheType, that.cacheType)
                && Objects.equals(url, that.url)
                && Objects.equals(this.getProtocol(), that.getProtocol())
                && Objects.equals(this.getUpstreamUrl(), that.getUpstreamUrl())
                && Objects.equals(this.isGray(), that.isGray())
                && Objects.equals(database, that.database);
    }

    @Override
    public int hashCode() {
        return Objects.hash(cacheType, url, this.isGray(), database);
    }

    @Override
    public String toString() {
        return "CacheUpstream{"
                + "cacheType='" + cacheType
                + "', url='" + url
                + "', protocol='" + this.getProtocol()
                + ", upstreamUrl='" + this.getUpstreamUrl()
                + "', gray=" + this.isGray()
                + ", database=" + database
                + "'}";
    }

    /**
     * class builder.
     */
    public static final class Builder {

        /**
         * upstreamHost.
         */
        private String upstreamHost;

        /**
         * protocol.
         */
        private String protocol;

        /**
         * upstreamUrl.
         */
        private String upstreamUrl;

        /**
         * weight.
         */
        private int weight;

        /**
         * status.
         */
        private boolean statusSet;

        /**
         * status.
         */
        private boolean statusValue;

        /**
         * timestamp.
         */
        private long timestamp;

        /**
         * cacheType.
         */
        private String cacheType;

        /**
         * url.
         */
        private String url;

        /**
         * password.
         */
        private String password;

        /**
         * database.
         */
        private String database;

        /**
         * master.
         */
        private String master;

        /**
         * mode.
         */
        private String mode;

        /**
         * maxIdle.
         */
        private int maxIdle;

        /**
         * minIdle.
         */
        private int minIdle;

        /**
         * maxActive.
         */
        private int maxActive;

        /**
         * maxWait.
         */
        private int maxWait;

        /**
         * no args constructor.
         */
        public Builder() {
        }

        public CacheUpstream build() {
            return new CacheUpstream(this);
        }

        /**
         * build upstreamHost.
         *
         * @param upstreamHost upstreamHost
         * @return this
         */
        public Builder upstreamHost(final String upstreamHost) {
            this.upstreamHost = upstreamHost;
            return this;
        }

        /**
         * build protocol.
         *
         * @param protocol protocol
         * @return this
         */
        public Builder protocol(final String protocol) {
            this.protocol = protocol;
            return this;
        }

        /**
         * build upstreamUrl.
         *
         * @param upstreamUrl upstreamUrl
         * @return this
         */
        public Builder upstreamUrl(final String upstreamUrl) {
            this.upstreamUrl = upstreamUrl;
            return this;
        }

        /**
         * build weight.
         *
         * @param weight weight
         * @return this
         */
        public Builder weight(final int weight) {
            this.weight = weight;
            return this;
        }

        /**
         * build status.
         *
         * @param status status
         * @return this
         */
        public Builder status(final boolean status) {
            this.statusValue = status;
            this.statusSet = true;
            return this;
        }

        /**
         * build timestamp.
         *
         * @param timestamp timestamp
         * @return this
         */
        public Builder timestamp(final long timestamp) {
            this.timestamp = timestamp;
            return this;
        }

        /**
         * build cacheType.
         *
         * @param cacheType cacheType
         * @return this
         */
        public Builder cacheType(final String cacheType) {
            this.cacheType = cacheType;
            return this;
        }

        /**
         * build url.
         *
         * @param url url
         * @return this
         */
        public Builder url(final String url) {
            this.url = url;
            return this;
        }

        /**
         * build database.
         *
         * @param database database
         * @return this
         */
        public Builder database(final String database) {
            this.database = database;
            return this;
        }
    }

}
