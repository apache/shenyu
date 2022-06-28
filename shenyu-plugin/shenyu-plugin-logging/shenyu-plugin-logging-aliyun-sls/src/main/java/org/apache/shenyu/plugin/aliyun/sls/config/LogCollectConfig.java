package org.apache.shenyu.plugin.aliyun.sls.config;

/**
 * LogCollectConfig.
 */
public class LogCollectConfig {

    private GlobalLogConfig globalLogConfig;

    /**
     * get global log config.
     *
     * @return global log config
     */
    public GlobalLogConfig getGlobalLogConfig() {
        return globalLogConfig;
    }

    /**
     * set global log config.
     *
     * @param globalLogConfig global log config.
     */
    public void setGlobalLogConfig(final GlobalLogConfig globalLogConfig) {
        this.globalLogConfig = globalLogConfig;
    }

    /**
     * global log config.
     */
    public static class GlobalLogConfig {

        private String host;

        private String accessId;

        private String accessKey;

        private String projectName;

        private String logStoreName;

        private Integer ttlInDay = 3;

        private Integer shardCount = 10;

        private String topic;

        private String sampleRate = "1";

        private String compressAlg;

        /**
         * default 512KB.
         */
        private int maxResponseBody = 524288;

        /**
         * default 512kb.
         */
        private int maxRequestBody = 524288;

        private int bufferQueueSize = 50000;

        /**
         * get sample rate.
         *
         * @return sample
         */
        public String getSampleRate() {
            return sampleRate;
        }

        /**
         * set sample rate.
         *
         * @param sampleRate rate
         */
        public void setSampleRate(final String sampleRate) {
            this.sampleRate = sampleRate;
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
         * get max response org.apache.shenyu.plugin.logging.body.
         *
         * @return get max response org.apache.shenyu.plugin.logging.body
         */
        public int getMaxResponseBody() {
            return maxResponseBody;
        }

        /**
         * set max response org.apache.shenyu.plugin.logging.body.
         *
         * @param maxResponseBody max response org.apache.shenyu.plugin.logging.body
         */
        public void setMaxResponseBody(final int maxResponseBody) {
            this.maxResponseBody = maxResponseBody;
        }

        /**
         * get max request org.apache.shenyu.plugin.logging.body.
         *
         * @return max request org.apache.shenyu.plugin.logging.body
         */
        public int getMaxRequestBody() {
            return maxRequestBody;
        }

        /**
         * set max request org.apache.shenyu.plugin.logging.body.
         *
         * @param maxRequestBody max request org.apache.shenyu.plugin.logging.body
         */
        public void setMaxRequestBody(final int maxRequestBody) {
            this.maxRequestBody = maxRequestBody;
        }

        /**
         * get buffer queue size.
         *
         * @return buffer queue size
         */
        public int getBufferQueueSize() {
            return bufferQueueSize;
        }

        /**
         * set buffer queue size.
         *
         * @param bufferQueueSize buffer queue size
         */
        public void setBufferQueueSize(final int bufferQueueSize) {
            this.bufferQueueSize = bufferQueueSize;
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
         * @param host host name
         */
        public void setHost(final String host) {
            this.host = host;
        }

        public String getAccessId() {
            return accessId;
        }

        public void setAccessId(String accessId) {
            this.accessId = accessId;
        }

        public String getAccessKey() {
            return accessKey;
        }

        public void setAccessKey(String accessKey) {
            this.accessKey = accessKey;
        }

        public String getProjectName() {
            return projectName;
        }

        public void setProjectName(String projectName) {
            this.projectName = projectName;
        }

        public String getLogStoreName() {
            return logStoreName;
        }

        public void setLogStoreName(String logStoreName) {
            this.logStoreName = logStoreName;
        }

        public Integer getTtlInDay() {
            return ttlInDay;
        }

        public void setTtlInDay(Integer ttlInDay) {
            this.ttlInDay = ttlInDay;
        }

        public Integer getShardCount() {
            return shardCount;
        }

        public void setShardCount(Integer shardCount) {
            this.shardCount = shardCount;
        }

        public String getTopic() {
            return topic;
        }

        public void setTopic(String topic) {
            this.topic = topic;
        }
    }
}
