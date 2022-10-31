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

package org.apache.shenyu.plugin.logging.rocketmq.config;

import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;

import java.util.Objects;
import java.util.Optional;

/**
 * log collect config, include rocketmq config.
 * Topic and nameserver must be included, and others are optional.
 * We should operate the configuration through admin instead of the configuration file.
 */
public class RocketMQLogCollectConfig {

    public static final RocketMQLogCollectConfig INSTANCE = new RocketMQLogCollectConfig();

    private RocketMQLogConfig rocketMQLogConfig;

    /**
     * get global log config.
     *
     * @return global log config
     */
    public RocketMQLogConfig getRocketMQLogConfig() {
        return Optional.ofNullable(rocketMQLogConfig).orElse(new RocketMQLogConfig());
    }

    /**
     * set global log config.
     *
     * @param rocketMQLogConfig global log config.
     */
    public void setRocketMQLogConfig(final RocketMQLogConfig rocketMQLogConfig) {
        this.rocketMQLogConfig = rocketMQLogConfig;
    }

    /**
     * global log config.
     */
    public static class RocketMQLogConfig extends GenericGlobalConfig {

        private String compressAlg;

        private String topic;

        private String namesrvAddr;

        private String producerGroup;

        private String accessKey;

        private String secretKey;

        /**
         * whether compress.
         *
         * @return compress or not
         */
        public String getCompressAlg() {
            return compressAlg;
        }

        /**
         * set compress.
         *
         * @param compressAlg compress alg.
         */
        public void setCompressAlg(final String compressAlg) {
            this.compressAlg = compressAlg;
        }

        /**
         * get message queue topic.
         *
         * @return message queue topic
         */
        public String getTopic() {
            return topic;
        }

        /**
         * topic,used for message queue.
         *
         * @param topic mq topic
         */
        public void setTopic(final String topic) {
            this.topic = topic;
        }

        /**
         * get rocketmq nameserver address.
         *
         * @return rocketmq nameserver address
         */
        public String getNamesrvAddr() {
            return namesrvAddr;
        }

        /**
         * set rocketmq nameserver address.
         *
         * @param namesrvAddr rocketmq nameserver address
         */
        public void setNamesrvAddr(final String namesrvAddr) {
            this.namesrvAddr = namesrvAddr;
        }

        /**
         * get producer group.
         *
         * @return producer group
         */
        public String getProducerGroup() {
            return producerGroup;
        }

        /**
         * set producer group.
         *
         * @param producerGroup producer group
         */
        public void setProducerGroup(final String producerGroup) {
            this.producerGroup = producerGroup;
        }

        /**
         * get accessKey.
         *
         * @return accessKey
         */
        public String getAccessKey() {
            return accessKey;
        }

        /**
         * set accessKey.
         *
         * @param accessKey accessKey
         */
        public void setAccessKey(final String accessKey) {
            this.accessKey = accessKey;
        }

        /**
         * get secretKey.
         *
         * @return secretKey
         */
        public String getSecretKey() {
            return secretKey;
        }

        /**
         * set secretKey.
         *
         * @param secretKey secretKey
         */
        public void setSecretKey(final String secretKey) {
            this.secretKey = secretKey;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return Boolean.TRUE;
            }

            if (o == null || getClass() != o.getClass()) {
                return Boolean.FALSE;
            }

            RocketMQLogConfig that = (RocketMQLogConfig) o;
            return Objects.equals(getTopic(), that.getTopic())
                    && Objects.equals(getCompressAlg(), that.getCompressAlg())
                    && Objects.equals(getNamesrvAddr(), that.getNamesrvAddr())
                    && Objects.equals(getProducerGroup(), that.getProducerGroup())
                    && Objects.equals(getAccessKey(), that.getAccessKey())
                    && Objects.equals(getSecretKey(), that.getSecretKey())
                    && Objects.equals(getSampleRate(), that.getSampleRate())
                    && Objects.equals(getBufferQueueSize(), that.getBufferQueueSize())
                    && Objects.equals(getMaxResponseBody(), that.getMaxRequestBody())
                    && Objects.equals(getMaxRequestBody(), that.getMaxResponseBody());
        }

        @Override
        public int hashCode() {
            return Objects.hash(topic, compressAlg, secretKey, accessKey, namesrvAddr, producerGroup);
        }
    }

    /**
     * api log config.
     */
    public static class LogApiConfig extends GenericApiConfig {
    }
}
