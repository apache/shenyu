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

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class HuaweiLtsLogCollectClient extends AbstractLogConsumeClient<HuaweiLogCollectConfig.HuaweiLtsLogConfig, ShenyuRequestLog> {

    private Producer producer;

    private String logGroupId;

    private String logStreamId;

    private String projectId;

    private ThreadPoolExecutor threadExecutor;

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
                // 华为云帐号的项目ID（project id）
                .setProjectId(projectId)
                // 华为云帐号的AK
                .setAccessKeyId(accessKeyId)
                // 华为云帐号的SK
                .setAccessKeySecret(accessKeySecret)
                // 云日志服务的区域
                .setRegionName(regionName)
                // 单个Appender能缓存的日志大小上限
                .setTotalSizeInBytes(huaweiLtsLogConfig.getTotalSizeInBytes())
                // producer发送日志时阻塞时间
                .setMaxBlockMs(huaweiLtsLogConfig.getMaxBlockMs())
                // producer发送单批日志量上限
                .setBatchSizeThresholdInBytes(huaweiLtsLogConfig.getBatchSizeThresholdInBytes())
                // producer发送单批日志条数上限
                .setBatchCountThreshold(huaweiLtsLogConfig.getBatchCountThreshold())
                // producer发送单批日志等待时间
                .setLingerMs(huaweiLtsLogConfig.getLingerMs())
                // producer发送日志失败后重试次数
                .setRetries(huaweiLtsLogConfig.getRetries())
                // 首次重试的退避时间
                .setBaseRetryBackoffMs(huaweiLtsLogConfig.getBaseRetryBackoffMs())
                // 重试的最大退避时间
                .setMaxRetryBackoffMs(huaweiLtsLogConfig.getMaxRetryBackoffMs())
                // 默认false, true: 可以跨云上报日志, false: 仅能在华为云ecs主机上报日志
                .setEnableLocalTest(Boolean.parseBoolean(huaweiLtsLogConfig.getEnableLocalTest()))
                // 超过1M的日志, 拆分后丢弃大于1M的数据
                .setGiveUpExtraLongSingleLog(Boolean.parseBoolean(huaweiLtsLogConfig.getEnableLocalTest()))
                .builder();
        this.producer = appender.getProducer();

        threadExecutor = createThreadPoolExecutor(huaweiLtsLogConfig.getIoThreadCount());
    }

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
     * send log to Tencent cls.
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
            throw new RuntimeException(e);
        } catch (ProducerException e) {
            throw new RuntimeException(e);
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
