package org.apache.shenyu.plugin.huawei.lts.config;

import org.apache.shenyu.plugin.huawei.lts.client.HuaweiLtsLogCollectClient;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;

import java.util.Optional;

public class HuaweiLogCollectConfig {
    public static final HuaweiLogCollectConfig INSTANCE = new HuaweiLogCollectConfig();

    private HuaweiLtsLogConfig huaweiLtsLogConfig;

    /**
     * get Huawei lts log config.
     *
     * @return global log config
     */
    public HuaweiLtsLogConfig getHuaweiLogCollectConfig() {
        return Optional.ofNullable(huaweiLtsLogConfig).orElse(new HuaweiLtsLogConfig());
    }

    /**
     * set Huawei lts log config.
     *
     * @param huaweiLtsLogConfig Tencent cls log config.
     */
    public void setHuaweiLtsLogConfig(final HuaweiLtsLogConfig huaweiLtsLogConfig) {
        this.huaweiLtsLogConfig = huaweiLtsLogConfig;
    }

    /**
     * global log config.
     * please seee https://github.com/TencentCloud/tencentcloud-cls-sdk-java/blob/main/README.md#%E9%85%8D%E7%BD%AE%E5%8F%82%E6%95%B0%E8%AF%A6%E8%A7%A3
     */
    public static class HuaweiLtsLogConfig extends GenericGlobalConfig {

        private String projectId;

        private String logGroupId;

        private String logStreamId;

        private String accessKeyId;

        private String accessKeySecret;

        private String regionName;

        private Integer totalSizeInBytes=104857600;

        private Long maxBlockMs=0L;

        private Integer ioThreadCount=1;

        private Integer batchSizeThresholdInBytes=524288;

        private Integer batchCountThreshold=4096;

        private Integer lingerMs=2000;

        private Integer retries=1;

        private Long baseRetryBackoffMs=100L;

        private Long maxRetryBackoffMs=100L;

        private String enableLocalTest="true";

        private String setGiveUpExtraLongSingleLog="true";

        public String getProjectId() {
            return projectId;
        }

        public void setProjectId(String projectId) {
            this.projectId = projectId;
        }

        public String getLogGroupId() {
            return logGroupId;
        }

        public void setLogGroupId(String logGroupId) {
            this.logGroupId = logGroupId;
        }

        public String getLogStreamId() {
            return logStreamId;
        }

        public void setLogStreamId(String logStreamId) {
            this.logStreamId = logStreamId;
        }

        public String getAccessKeyId() {
            return accessKeyId;
        }

        public void setAccessKeyId(String accessKeyId) {
            this.accessKeyId = accessKeyId;
        }

        public String getAccessKeySecret() {
            return accessKeySecret;
        }

        public void setAccessKeySecret(String accessKeySecret) {
            this.accessKeySecret = accessKeySecret;
        }

        public String getRegionName() {
            return regionName;
        }

        public void setRegionName(String regionName) {
            this.regionName = regionName;
        }

        public Integer getTotalSizeInBytes() {
            return totalSizeInBytes;
        }

        public void setTotalSizeInBytes(Integer totalSizeInBytes) {
            this.totalSizeInBytes = totalSizeInBytes;
        }

        public Long getMaxBlockMs() {
            return maxBlockMs;
        }

        public void setMaxBlockMs(Long maxBlockMs) {
            this.maxBlockMs = maxBlockMs;
        }

        public Integer getIoThreadCount() {
            return ioThreadCount;
        }

        public void setIoThreadCount(Integer ioThreadCount) {
            this.ioThreadCount = ioThreadCount;
        }

        public Integer getBatchSizeThresholdInBytes() {
            return batchSizeThresholdInBytes;
        }

        public void setBatchSizeThresholdInBytes(Integer batchSizeThresholdInBytes) {
            this.batchSizeThresholdInBytes = batchSizeThresholdInBytes;
        }

        public Integer getBatchCountThreshold() {
            return batchCountThreshold;
        }

        public void setBatchCountThreshold(Integer batchCountThreshold) {
            this.batchCountThreshold = batchCountThreshold;
        }

        public Integer getLingerMs() {
            return lingerMs;
        }

        public void setLingerMs(Integer lingerMs) {
            this.lingerMs = lingerMs;
        }

        public Integer getRetries() {
            return retries;
        }

        public void setRetries(Integer retries) {
            this.retries = retries;
        }

        public Long getBaseRetryBackoffMs() {
            return baseRetryBackoffMs;
        }

        public void setBaseRetryBackoffMs(Long baseRetryBackoffMs) {
            this.baseRetryBackoffMs = baseRetryBackoffMs;
        }

        public Long getMaxRetryBackoffMs() {
            return maxRetryBackoffMs;
        }

        public void setMaxRetryBackoffMs(Long maxRetryBackoffMs) {
            this.maxRetryBackoffMs = maxRetryBackoffMs;
        }

        public String getEnableLocalTest() {
            return enableLocalTest;
        }

        public void setEnableLocalTest(String enableLocalTest) {
            this.enableLocalTest = enableLocalTest;
        }

        public String getSetGiveUpExtraLongSingleLog() {
            return setGiveUpExtraLongSingleLog;
        }

        public void setSetGiveUpExtraLongSingleLog(String setGiveUpExtraLongSingleLog) {
            this.setGiveUpExtraLongSingleLog = setGiveUpExtraLongSingleLog;
        }
    }
}
