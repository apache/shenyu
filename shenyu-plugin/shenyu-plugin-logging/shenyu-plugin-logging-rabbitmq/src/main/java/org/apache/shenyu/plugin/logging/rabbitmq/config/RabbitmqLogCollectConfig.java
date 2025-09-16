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

package org.apache.shenyu.plugin.logging.rabbitmq.config;

import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;

import jakarta.annotation.Nullable;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * log collect config, include rabbitmq config.
 * Operate the configuration through admin instead of the configuration file.
 */
public class RabbitmqLogCollectConfig {

    public static final RabbitmqLogCollectConfig INSTANCE = new RabbitmqLogCollectConfig();

    private RabbitmqLogConfig rabbitmqLogConfig;

    /**
     * get rabbitmq log config.
     *
     * @return rabbitmq log config.
     */
    public RabbitmqLogConfig getRabbitmqLogConfig() {
        return Optional.ofNullable(rabbitmqLogConfig).orElse(new RabbitmqLogConfig());
    }

    /**
     * set rabbitmq log config.
     *
     * @param rabbitmqLogConfig rabbitmq log config
     */
    public void setRabbitmqLogConfig(final RabbitmqLogConfig rabbitmqLogConfig) {
        this.rabbitmqLogConfig = rabbitmqLogConfig;
    }

    /**
     * global log config.
     */
    public static class RabbitmqLogConfig extends GenericGlobalConfig {

        private String queueName;

        private String exchangeName;

        private String host;

        private Integer port;

        private String username;

        private String password;

        private String routingKey;

        private String exchangeType;

        private String virtualHost;

        private Boolean durable;

        private Boolean exclusive;

        private Boolean autoDelete;

        private Map<String, Object> args;

        /**
         * get Durable.
         *
         * @return Durable
         */
        public Boolean getDurable() {
            return durable;
        }

        /**
         * set Durable.
         *
         * @param durable Durable
         */
        public void setDurable(final Boolean durable) {
            this.durable = durable;
        }

        /**
         * get Exclusive.
         *
         * @return Exclusive
         */
        public Boolean getExclusive() {
            return exclusive;
        }

        /**
         * set Exclusive.
         *
         * @param exclusive Exclusive
         */
        public void setExclusive(final Boolean exclusive) {
            this.exclusive = exclusive;
        }

        /**
         * get AutoDelete.
         *
         * @return AutoDelete
         */
        public Boolean getAutoDelete() {
            return autoDelete;
        }

        /**
         * set AutoDelete.
         *
         * @param autoDelete AutoDelete
         */
        public void setAutoDelete(final Boolean autoDelete) {
            this.autoDelete = autoDelete;
        }

        /**
         * get Rabbitmq Args.
         *
         * @return Rabbitmq Args
         */
        public Map<String, Object> getArgs() {
            return args;
        }

        /**
         * set Rabbitmq Args.
         *
         * @param args Rabbitmq Args
         */
        public void setArgs(@Nullable final Map<String, Object> args) {
            this.args = args;
        }

        /**
         * get VirtualHost.
         *
         * @return VirtualHost
         */
        public String getVirtualHost() {
            return virtualHost;
        }

        /**
         * set VirtualHost.
         *
         * @param virtualHost VirtualHost
         */
        public void setVirtualHost(final String virtualHost) {
            this.virtualHost = virtualHost;
        }

        /**
         * get exchangeType.
         *
         * @return exchangeType
         */
        public String getExchangeType() {
            return exchangeType;
        }

        /**
         * set exchangeType.
         *
         * @param exchangeType exchangeType
         */
        public void setExchangeType(final String exchangeType) {
            this.exchangeType = exchangeType;
        }

        /**
         * Get RabbitMQ routingKey.
         *
         * @return RabbitMQ routingKey
         */
        public String getRoutingKey() {
            return routingKey;
        }

        /**
         * Set RabbitMQ routingKey.
         *
         * @param routingKey RabbitMQ routingKey
         */
        public void setRoutingKey(final String routingKey) {
            this.routingKey = routingKey;
        }

        /**
         * Get RabbitMQ queue name.
         *
         * @return Queue name
         */
        public String getQueueName() {
            return queueName;
        }

        /**
         * Set RabbitMQ queue name.
         *
         * @param queueName Queue name
         */
        public void setQueueName(final String queueName) {
            this.queueName = queueName;
        }

        /**
         * Get RabbitMQ exchange name.
         *
         * @return Exchange name
         */
        public String getExchangeName() {
            return exchangeName;
        }

        /**
         * Set RabbitMQ exchange name.
         *
         * @param exchangeName Exchange name
         */
        public void setExchangeName(final String exchangeName) {
            this.exchangeName = exchangeName;
        }

        /**
         * Get RabbitMQ host.
         *
         * @return Host
         */
        public String getHost() {
            return host;
        }

        /**
         * Set RabbitMQ host.
         *
         * @param host Host
         */
        public void setHost(final String host) {
            this.host = host;
        }

        /**
         * Get RabbitMQ port.
         *
         * @return Port
         */
        public Integer getPort() {
            return port;
        }

        /**
         * Set RabbitMQ port.
         *
         * @param port Port
         */
        public void setPort(final Integer port) {
            this.port = port;
        }

        /**
         * Get RabbitMQ username.
         *
         * @return Username
         */
        public String getUsername() {
            return username;
        }

        /**
         * Set RabbitMQ username.
         *
         * @param username Username
         */
        public void setUsername(final String username) {
            this.username = username;
        }

        /**
         * Get RabbitMQ password.
         *
         * @return Password
         */
        public String getPassword() {
            return password;
        }

        /**
         * Set RabbitMQ password.
         *
         * @param password Password
         */
        public void setPassword(final String password) {
            this.password = password;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return Boolean.TRUE;
            }
            if (Objects.isNull(o) || getClass() != o.getClass()) {
                return Boolean.FALSE;
            }
            RabbitmqLogConfig that = (RabbitmqLogConfig) o;

            return Objects.equals(getRoutingKey(), that.getRoutingKey())
                    && Objects.equals(getQueueName(), that.getQueueName())
                    && Objects.equals(getExchangeName(), that.getExchangeName())
                    && Objects.equals(getHost(), that.getHost())
                    && Objects.equals(getPort(), that.getPort())
                    && Objects.equals(getUsername(), that.getUsername())
                    && Objects.equals(getPassword(), that.getPassword())
                    && Objects.equals(getExchangeType(), that.getExchangeType())
                    && Objects.equals(getVirtualHost(), that.getVirtualHost())
                    && Objects.equals(getDurable(), that.getDurable())
                    && Objects.equals(getExclusive(), that.getExclusive())
                    && Objects.equals(getAutoDelete(), that.getAutoDelete())
                    && Objects.equals(getArgs(), that.getArgs())
                    && Objects.equals(getSampleRate(), that.getSampleRate())
                    && Objects.equals(getBufferQueueSize(), that.getBufferQueueSize())
                    && Objects.equals(getMaxResponseBody(), that.getMaxResponseBody())
                    && Objects.equals(getMaxRequestBody(), that.getMaxRequestBody());
        }

        @Override
        public int hashCode() {
            return Objects.hash(routingKey, queueName, exchangeName, host, port, exchangeType, virtualHost, durable, autoDelete, exchangeType, args,
                    username, password, getSampleRate(), getBufferQueueSize(), getMaxResponseBody(), getMaxRequestBody());
        }
    }

    /**
     * api log config.
     */
    public static class LogApiConfig extends GenericApiConfig {
    }
}
