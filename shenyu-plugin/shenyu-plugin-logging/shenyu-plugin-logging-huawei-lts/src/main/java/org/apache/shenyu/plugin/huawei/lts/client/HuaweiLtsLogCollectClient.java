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

package org.apache.shenyu.plugin.huawei.lts.client;

import com.alibaba.fastjson.JSONObject;
import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import com.huaweicloud.lts.appender.JavaSDKAppender;
import com.huaweicloud.lts.producer.Producer;
import com.huaweicloud.lts.producer.Result;
import com.huaweicloud.lts.producer.exception.LogSizeTooLargeException;
import com.huaweicloud.lts.producer.exception.MaxBatchCountExceedException;
import com.huaweicloud.lts.producer.exception.ProducerException;
import com.huaweicloud.lts.producer.exception.ResultFailedException;
import com.huaweicloud.lts.producer.model.log.LogContent;
import com.huaweicloud.lts.producer.model.log.LogItem;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.plugin.huawei.lts.config.HuaweiLogCollectConfig;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import jakarta.annotation.Nullable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Huawei lts log Collect client.
 */
public class HuaweiLtsLogCollectClient extends AbstractLogConsumeClient<HuaweiLogCollectConfig.HuaweiLtsLogConfig, ShenyuRequestLog> {

    private Producer producer;

    private String logGroupId;

    private String logStreamId;

    private String projectId;

    private ThreadPoolExecutor threadExecutor;

    /**
     * init Huawei lts client.
     *
     * @param huaweiLtsLogConfig shenyu log config
     */
    @Override
    public void initClient0(@NonNull final HuaweiLogCollectConfig.HuaweiLtsLogConfig huaweiLtsLogConfig) {
        final String accessKeyId = huaweiLtsLogConfig.getAccessKeyId();
        final String accessKeySecret = huaweiLtsLogConfig.getAccessKeySecret();
        final String regionName = huaweiLtsLogConfig.getRegionName();
        this.projectId = huaweiLtsLogConfig.getProjectId();
        this.logGroupId = huaweiLtsLogConfig.getLogGroupId();
        this.logStreamId = huaweiLtsLogConfig.getLogStreamId();
        if (StringUtils.isBlank(accessKeyId) || StringUtils.isBlank(accessKeySecret) || StringUtils.isBlank(projectId)
                || StringUtils.isBlank(regionName) || StringUtils.isBlank(logGroupId) || StringUtils.isBlank(logStreamId)) {
            LOG.error("init Huawei lts client error, please check projectId, accessKeyId, accessKeySecret, regionName, logGroupId or logStreamId");
            return;
        }
        JavaSDKAppender appender = JavaSDKAppender.custom()
                .setProjectId(projectId)
                .setAccessKeyId(accessKeyId)
                .setAccessKeySecret(accessKeySecret)
                .setRegionName(regionName)
                .setTotalSizeInBytes(huaweiLtsLogConfig.getTotalSizeInBytes())
                .setMaxBlockMs(huaweiLtsLogConfig.getMaxBlockMs())
                .setBatchSizeThresholdInBytes(huaweiLtsLogConfig.getBatchSizeThresholdInBytes())
                .setBatchCountThreshold(huaweiLtsLogConfig.getBatchCountThreshold())
                .setLingerMs(huaweiLtsLogConfig.getLingerMs())
                .setRetries(huaweiLtsLogConfig.getRetries())
                .setBaseRetryBackoffMs(huaweiLtsLogConfig.getBaseRetryBackoffMs())
                .setMaxRetryBackoffMs(huaweiLtsLogConfig.getMaxRetryBackoffMs())
                .setEnableLocalTest(Boolean.parseBoolean(huaweiLtsLogConfig.getEnableLocalTest()))
                .setGiveUpExtraLongSingleLog(Boolean.parseBoolean(huaweiLtsLogConfig.getEnableLocalTest()))
                .builder();
        this.producer = appender.getProducer();

        threadExecutor = createThreadPoolExecutor(huaweiLtsLogConfig.getIoThreadCount());
    }

    /**
     * Huawei lts consume.
     *
     * @param logs list of log
     */
    @Override
    public void consume0(@NonNull final List<ShenyuRequestLog> logs) throws Exception {
        logs.forEach(this::sendLog);
    }

    @Override
    public void close0() throws Exception {
        if (Objects.nonNull(producer)) {
            try {
                producer.close();

            } catch (InterruptedException | ProducerException e) {
                LOG.error("Close producer error.");
            }
        }
    }

    /**
     * send log to Huawei lts.
     *
     * @param log log
     */
    private void sendLog(final ShenyuRequestLog log) {
        final List<LogItem> logItems = new ArrayList<>();
        final List<LogItem> logItemList = new ArrayList<>();
        LogItem logItem = new LogItem();
        logItem.setTenantProjectId(projectId);
        logItems.add(logItem);
        logItem.setLabels(JSONObject.toJSONString(new HashMap<>()));
        List<LogContent> contents = new ArrayList<>();
        LogContent logContent = new LogContent();
        logContent.setLogTimeNs(System.currentTimeMillis() * 1000000L + System.nanoTime() % 1000000L);
        logContent.setLog(log.toString());
        contents.add(logContent);
        logItem.setContents(contents);
        logItemList.add(logItem);

        try {
            final ListenableFuture<Result> f = producer.send(logGroupId, logStreamId, logItemList);
            Futures.addCallback(f, new ProducerFutureCallback(logGroupId, logStreamId), threadExecutor);
        } catch (InterruptedException e) {
            LOG.warn("The current thread has been interrupted during send logs.");
        } catch (ProducerException e) {
            if (e instanceof MaxBatchCountExceedException) {
                LOG.error("The logs exceeds the maximum batch count, e={}", e.getMessage());
            } else if (e instanceof LogSizeTooLargeException) {
                LOG.error("The size of log is larger than the maximum allowable size, e={}", e.getMessage());
            } else {
                LOG.error("Failed to send logs, e={}", e.getMessage());
            }
        }
    }

    /**
     * create send log queue.
     *
     * @param sendThreadCount sendThreadCount
     * @return ThreadPoolExecutor
     */
    private static ThreadPoolExecutor createThreadPoolExecutor(final int sendThreadCount) {
        int threadCount = sendThreadCount;
        if (threadCount > GenericLoggingConstant.MAX_ALLOW_THREADS) {
            LOG.warn("send thread count number too large!");
            threadCount = GenericLoggingConstant.MAX_ALLOW_THREADS;
        }
        return new ThreadPoolExecutor(threadCount, GenericLoggingConstant.MAX_ALLOW_THREADS, 60000L, TimeUnit.MICROSECONDS,
                new LinkedBlockingQueue<>(GenericLoggingConstant.MAX_QUEUE_NUMBER), ShenyuThreadFactory.create("shenyu-huawei-lts", true),
                new ThreadPoolExecutor.AbortPolicy());
    }

    /**
     * Producer Future Callback.
     */
    private static final class ProducerFutureCallback implements FutureCallback<Result> {

        private static final Logger LOGGER = LoggerFactory.getLogger(ProducerFutureCallback.class);

        private final String logGroupId;

        private final String logStreamId;

        ProducerFutureCallback(final String logGroupId, final String logStreamId) {
            this.logGroupId = logGroupId;
            this.logStreamId = logStreamId;
        }

        @Override
        public void onSuccess(@Nullable final Result result) {
            LOGGER.info("Send logs to Huawei lts successfully.");
        }

        @Override
        public void onFailure(final Throwable throwable) {
            if (throwable instanceof ResultFailedException) {
                Result result = ((ResultFailedException) throwable).getResult();
                LOGGER.error("Failed to send logs, logGroupId={}, logStreamId={}, result={}", logGroupId, logStreamId, result);
            } else {
                LOGGER.error("Failed to send log, e={}", throwable.getMessage());
            }
        }
    }
}
