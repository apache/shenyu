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

package org.apache.shenyu.infra.etcd.config;

import java.util.StringJoiner;

/**
 * The type etcd configuration.
 */
public class EtcdConfig {

    private String url;

    private Integer sessionTimeout;

    private Integer connectionTimeout;

    private String serializer;

    public EtcdConfig(final String url, final Integer session,
                      final Integer connection, final String serializer) {

        this.url = url;
        this.sessionTimeout = session;
        this.connectionTimeout = connection;
        this.serializer = serializer;
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
     * Get sessionTimeout.
     *
     * @return sessionTimeout
     */
    public Integer getSessionTimeout() {
        return sessionTimeout;
    }

    /**
     * Set sessionTimeout.
     *
     * @param sessionTimeout sessionTimeout
     */
    public void setSessionTimeout(final Integer sessionTimeout) {
        this.sessionTimeout = sessionTimeout;
    }

    /**
     * Get connectionTimeout.
     *
     * @return connectionTimeout
     */
    public Integer getConnectionTimeout() {
        return connectionTimeout;
    }

    /**
     * Set connectionTimeout.
     *
     * @param connectionTimeout connectionTimeout
     */
    public void setConnectionTimeout(final Integer connectionTimeout) {
        this.connectionTimeout = connectionTimeout;
    }

    /**
     * Get serializer.
     *
     * @return serializer
     */
    public String getSerializer() {
        return serializer;
    }

    /**
     * Set serializer.
     *
     * @param serializer serializer
     */
    public void setSerializer(final String serializer) {
        this.serializer = serializer;
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", EtcdConfig.class.getSimpleName() + "[", "]")
                .add("url='" + url + "'")
                .add("sessionTimeout=" + sessionTimeout)
                .add("connectionTimeout=" + connectionTimeout)
                .add("serializer='" + serializer + "'")
                .toString();
    }

    public static Builder builder() {

        return new Builder();
    }

    public static final class Builder {

        private String url;

        private Integer sessionTimeout;

        private Integer connectionTimeout;

        private String serializer;

        /**
         * private constructor. not allow to create instance.
         */
        private Builder() {
        }

        public Builder url(final String url) {
            this.url = url;
            return this;
        }

        public Builder sessionTimeout(final Integer sessionTimeout) {
            this.sessionTimeout = sessionTimeout;
            return this;
        }

        public Builder connectionTimeout(final Integer connectionTimeout) {
            this.connectionTimeout = connectionTimeout;
            return this;
        }

        public Builder serializer(final String serializer) {
            this.serializer = serializer;
            return this;
        }

        public EtcdConfig build() {

            return new EtcdConfig(url, sessionTimeout, connectionTimeout, serializer);
        }
    }

}
