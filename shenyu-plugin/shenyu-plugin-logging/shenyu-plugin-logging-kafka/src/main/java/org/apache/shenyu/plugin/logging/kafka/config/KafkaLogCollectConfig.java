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

package org.apache.shenyu.plugin.logging.kafka.config;

import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;

import java.util.Objects;
import java.util.Optional;

/**
 * log collect config, include kafka config.
 * Topic and nameserver must be included, and others are optional.
 * We should operate the configuration through admin instead of the configuration file.
 */
public class KafkaLogCollectConfig {

    public static final KafkaLogCollectConfig INSTANCE = new KafkaLogCollectConfig();

    private KafkaLogConfig kafkaLogConfig;

    /**
     * get kafka log config.
     *
     * @return kafka log config
     */
    public KafkaLogConfig getKafkaLogConfig() {
        return Optional.ofNullable(kafkaLogConfig).orElse(new KafkaLogConfig());
    }

    /**
     * set kafka log config.
     *
     * @param kafkaLogConfig kafka log config.
     */
    public void setKafkaLogConfig(final KafkaLogConfig kafkaLogConfig) {
        this.kafkaLogConfig = kafkaLogConfig;
    }

    /**
     * global log config.
     */
    public static class KafkaLogConfig extends GenericGlobalConfig {

        private String topic;

        private String namesrvAddr;

        private String producerGroup;

        private String compressAlg;

        private String securityProtocol;

        private String saslMechanism;

        private String userName;

        private String passWord;

        /**
         * get kafka securityProtocol.
         *
         * @return securityProtocol
         */
        public String getSecurityProtocol() {
            return securityProtocol;
        }

        /**
         * set kafka securityProtocol.
         *
         * @param securityProtocol securityProtocol
         */
        public void setSecurityProtocol(final String securityProtocol) {
            this.securityProtocol = securityProtocol;
        }

        /**
         * get Kafka saslMechanism.
         *
         * @return saslMechanism
         */
        public String getSaslMechanism() {
            return saslMechanism;
        }

        /**
         *set kafka saslMechanism.
         *
         * @param saslMechanism saslMechanism
         */
        public void setSaslMechanism(final String saslMechanism) {
            this.saslMechanism = saslMechanism;
        }

        /**
         * get kafka userName.
         *
         * @return userName
         */
        public String getUserName() {
            return userName;
        }

        /**
         * set kafka userName.
         *
         * @param userName userName
         */
        public void setUserName(final String userName) {
            this.userName = userName;
        }

        /**
         * get kafka passWord.
         *
         * @return passWord
         */
        public String getPassWord() {
            return passWord;
        }

        /**
         * set kafka pwd.
         *
         * @param passWord passWord
         */
        public void setPassWord(final String passWord) {
            this.passWord = passWord;
        }

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
         * get kafka nameserver address.
         *
         * @return kafka nameserver address
         */
        public String getNamesrvAddr() {
            return namesrvAddr;
        }

        /**
         * set kafka nameserver address.
         *
         * @param namesrvAddr kafka nameserver address
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

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return Boolean.TRUE;
            }

            if (o == null || getClass() != o.getClass()) {
                return Boolean.FALSE;
            }

            KafkaLogConfig that = (KafkaLogConfig) o;
            return Objects.equals(getTopic(), that.getTopic())
                    && Objects.equals(getCompressAlg(), that.getCompressAlg())
                    && Objects.equals(getNamesrvAddr(), that.getNamesrvAddr())
                    && Objects.equals(getProducerGroup(), that.getProducerGroup())
                    && Objects.equals(getSampleRate(), that.getSampleRate())
                    && Objects.equals(getBufferQueueSize(), that.getBufferQueueSize())
                    && Objects.equals(getMaxResponseBody(), that.getMaxRequestBody())
                    && Objects.equals(getMaxRequestBody(), that.getMaxResponseBody());
        }

        @Override
        public int hashCode() {
            return Objects.hash(topic, compressAlg, namesrvAddr, producerGroup);
        }
    }

    /**
     * api log config.
     */
    public static class LogApiConfig extends GenericApiConfig {
    }
}
