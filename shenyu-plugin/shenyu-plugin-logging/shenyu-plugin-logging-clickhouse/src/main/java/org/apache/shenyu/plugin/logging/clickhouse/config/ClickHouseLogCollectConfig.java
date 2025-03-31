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

package org.apache.shenyu.plugin.logging.clickhouse.config;

import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;

import java.util.Objects;
import java.util.Optional;

/**
 * ClickHouseLogCollectConfig.
 */
public class ClickHouseLogCollectConfig {

    public static final ClickHouseLogCollectConfig INSTANCE = new ClickHouseLogCollectConfig();

    private ClickHouseLogConfig clickHouseLogConfig;

    /**
     * get click house log config.
     *
     * @return click house log config.
     */
    public ClickHouseLogConfig getClickHouseLogConfig() {
        return Optional.ofNullable(clickHouseLogConfig).orElse(new ClickHouseLogConfig());
    }

    /**
     * set click house log config.
     *
     * @param clickHouseLogConfig log config.
     */
    public void setClickHouseLogConfig(final ClickHouseLogConfig clickHouseLogConfig) {
        this.clickHouseLogConfig = clickHouseLogConfig;
    }

    /**
     * click house log config.
     */
    public static class ClickHouseLogConfig extends GenericGlobalConfig {
        private String host;

        private String port;

        private String username;

        private String password;

        private String database;

        private String clusterName;

        private String engine;

        private String ttl;

        /**
         * get ttl.
         * @return ttl
         */
        public String getTtl() {
            return ttl;
        }

        /**
         * set ttl.
         * @param ttl ttl
         */
        public void setTtl(final String ttl) {
            this.ttl = ttl;
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
         * get Engine.
         * @return Engine
         */
        public String getEngine() {
            return engine;
        }

        /**
         * set engine.
         * @param engine engine
         */
        public void setEngine(final String engine) {
            this.engine = engine;
        }

        /**
         * get host.
         *
         * @return host
         */
        public String getHost() {
            return host;
        }

        /**
         * set host.
         *
         * @param host set host.
         */
        public void setHost(final String host) {
            this.host = host;
        }

        /**
         * get port.
         *
         * @return port
         */
        public String getPort() {
            return port;
        }

        /**
         * set port.
         *
         * @param port set port.
         */
        public void setPort(final String port) {
            this.port = port;
        }

        /**
         * get username.
         *
         * @return username.
         */
        public String getUsername() {
            return username;
        }

        /**
         * set username.
         *
         * @param username set username.
         */
        public void setUsername(final String username) {
            this.username = username;
        }

        /**
         * get password.
         *
         * @return password.
         */
        public String getPassword() {
            return password;
        }

        /**
         * set password.
         *
         * @param password password.
         */
        public void setPassword(final String password) {
            this.password = password;
        }

        /**
         * get database.
         *
         * @return database.
         */
        public String getDatabase() {
            return database;
        }

        /**
         * set database.
         *
         * @param database database.
         */
        public void setDatabase(final String database) {
            this.database = database;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return Boolean.TRUE;
            }
            if (Objects.isNull(o) || getClass() != o.getClass()) {
                return Boolean.FALSE;
            }
            ClickHouseLogConfig that = (ClickHouseLogConfig) o;

            return Objects.equals(getHost(), that.getHost())
                    && Objects.equals(getPort(), that.getPort())
                    && Objects.equals(getUsername(), that.getUsername())
                    && Objects.equals(getPassword(), that.getPassword())
                    && Objects.equals(getDatabase(), that.getDatabase())
                    && Objects.equals(getClusterName(), that.getClusterName())
                    && Objects.equals(getEngine(), that.getEngine())
                    && Objects.equals(getTtl(), that.getTtl())
                    && Objects.equals(getSampleRate(), that.getSampleRate())
                    && Objects.equals(getBufferQueueSize(), that.getBufferQueueSize())
                    && Objects.equals(getMaxResponseBody(), that.getMaxResponseBody())
                    && Objects.equals(getMaxRequestBody(), that.getMaxRequestBody());
        }

        @Override
        public int hashCode() {
            return Objects.hash(host, port, username, password, database, clusterName, engine, ttl);
        }
    }

}
