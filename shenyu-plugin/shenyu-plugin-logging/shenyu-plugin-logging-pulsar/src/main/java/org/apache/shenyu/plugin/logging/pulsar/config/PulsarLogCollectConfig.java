package org.apache.shenyu.plugin.logging.pulsar.config;

import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;

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
        public void setCompressAlg(String compressAlg) {
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
        public void setTopic(String topic) {
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
        public void setServiceUrl(String serviceUrl) {
            this.serviceUrl = serviceUrl;
        }
    }

    public static class LogApiConfig {

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
