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

package org.apache.shenyu.plugin.logging.pulsar.config;

import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;

import java.util.Objects;
import java.util.Optional;

public class PulsarLogCollectConfig {

    public static final PulsarLogCollectConfig INSTANCE = new PulsarLogCollectConfig();

    private PulsarLogConfig pulsarLogConfig;

    /**
     * get global log config.
     *
     * @return global log config
     */
    public PulsarLogConfig getPulsarLogConfig() {
        return Optional.ofNullable(pulsarLogConfig).orElse(new PulsarLogConfig());
    }

    /**
     * set global log config.
     *
     * @param pulsarLogConfig global log config
     */
    public void setPulsarLogConfig(final PulsarLogConfig pulsarLogConfig) {
        this.pulsarLogConfig = pulsarLogConfig;
    }

    public static class PulsarLogConfig extends GenericGlobalConfig {

        private String compressAlg;

        private String topic;

        private String serviceUrl;

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
         * get pulsar service URL.
         * @return pulsar service URL
         */
        public String getServiceUrl() {
            return serviceUrl;
        }

        /**
         * set pulsar service URL.
         * @param serviceUrl pulsar service URL
         */
        public void setServiceUrl(final String serviceUrl) {
            this.serviceUrl = serviceUrl;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return Boolean.TRUE;
            }

            if (o == null || getClass() != o.getClass()) {
                return Boolean.FALSE;
            }

            PulsarLogConfig that = (PulsarLogConfig) o;
            return Objects.equals(getTopic(), that.getTopic())
                    && Objects.equals(getCompressAlg(), that.getCompressAlg())
                    && Objects.equals(getServiceUrl(), that.getServiceUrl())
                    && Objects.equals(getSampleRate(), that.getSampleRate())
                    && Objects.equals(getBufferQueueSize(), that.getBufferQueueSize())
                    && Objects.equals(getMaxResponseBody(), that.getMaxRequestBody())
                    && Objects.equals(getMaxRequestBody(), that.getMaxResponseBody());
        }

        @Override
        public int hashCode() {
            return Objects.hash(topic, compressAlg, serviceUrl);
        }
    }

    public static class LogApiConfig extends GenericApiConfig {

        /**
         * 0 means never sample, 1 means always sample. Minimum probability is 0.01, or 1% of logging
         */
        private String sampleRate;

        /**
         * This topic is useful if you use message queuing to collect logs.
         */
        private String topic;

        /**
         * get sample rate.
         *
         * @return sample rate
         */
        public String getSampleRate() {
            return sampleRate;
        }

        /**
         * set sample rate.
         *
         * @param sampleRate sample rate
         */
        public void setSampleRate(final String sampleRate) {
            this.sampleRate = sampleRate;
        }

        /**
         * get mq topic.
         *
         * @return mq topic
         */
        public String getTopic() {
            return topic;
        }

        /**
         * set mq topic.
         *
         * @param topic mq topic
         */
        public void setTopic(final String topic) {
            this.topic = topic;
        }
    }
}
