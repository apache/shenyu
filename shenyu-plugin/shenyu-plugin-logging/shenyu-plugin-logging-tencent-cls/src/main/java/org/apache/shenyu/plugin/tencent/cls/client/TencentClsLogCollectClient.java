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

package org.apache.shenyu.plugin.tencent.cls.client;

import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import com.tencentcloudapi.cls.producer.AsyncProducerClient;
import com.tencentcloudapi.cls.producer.AsyncProducerConfig;
import com.tencentcloudapi.cls.producer.Result;
import com.tencentcloudapi.cls.producer.common.LogItem;
import com.tencentcloudapi.cls.producer.errors.LogSizeTooLargeException;
import com.tencentcloudapi.cls.producer.errors.MaxBatchCountExceedException;
import com.tencentcloudapi.cls.producer.errors.ProducerException;
import com.tencentcloudapi.cls.producer.errors.ResultFailedException;
import com.tencentcloudapi.cls.producer.util.NetworkUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.tencent.cls.config.TencentLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Tencent cls log Collect client.
 */
public class TencentClsLogCollectClient extends AbstractLogConsumeClient<TencentLogCollectConfig.TencentClsLogConfig, ShenyuRequestLog> {

    private AsyncProducerClient client;

    private String topic;

    private ThreadPoolExecutor threadExecutor;

    /**
     * init Tencent cls client.
     *
     * @param tencentClsLogConfig shenyu log config
     */
    @Override
    public void initClient0(@NonNull final TencentLogCollectConfig.TencentClsLogConfig tencentClsLogConfig) {
        String secretId = tencentClsLogConfig.getSecretId();
        String secretKey = tencentClsLogConfig.getSecretKey();
        String endpoint = tencentClsLogConfig.getEndpoint();
        this.topic = tencentClsLogConfig.getTopic();
        if (StringUtils.isBlank(secretId) || StringUtils.isBlank(secretKey) || StringUtils.isBlank(topic) || StringUtils.isBlank(endpoint)) {
            LOG.error("init Tencent cls client error, please check secretId, secretKey, topic or host");
            return;
        }

        // init AsyncProducerConfig, AsyncProducerClient
        final AsyncProducerConfig config = new AsyncProducerConfig(endpoint, secretId, secretKey, NetworkUtils.getLocalMachineIP());
        // Optional parameters
        Optional.ofNullable(tencentClsLogConfig.getTotalSizeInBytes()).map(Integer::valueOf).ifPresent(config::setTotalSizeInBytes);
        Optional.ofNullable(tencentClsLogConfig.getMaxSendThreadCount()).map(Integer::valueOf).ifPresent(config::setSendThreadCount);
        Optional.ofNullable(tencentClsLogConfig.getMaxBlockSec()).map(Long::valueOf).ifPresent(config::setMaxBlockMs);
        Optional.ofNullable(tencentClsLogConfig.getMaxBatchSize()).map(Integer::valueOf).ifPresent(config::setBatchSizeThresholdInBytes);
        Optional.ofNullable(tencentClsLogConfig.getMaxBatchCount()).map(Integer::valueOf).ifPresent(config::setBatchCountThreshold);
        Optional.ofNullable(tencentClsLogConfig.getLingerMs()).map(Integer::valueOf).ifPresent(config::setLingerMs);
        Optional.ofNullable(tencentClsLogConfig.getRetries()).map(Integer::valueOf).ifPresent(config::setRetries);
        Optional.ofNullable(tencentClsLogConfig.getMaxReservedAttempts()).map(Integer::valueOf).ifPresent(config::setMaxReservedAttempts);
        Optional.ofNullable(tencentClsLogConfig.getBaseRetryBackoffMs()).map(Long::valueOf).ifPresent(config::setBaseRetryBackoffMs);
        Optional.ofNullable(tencentClsLogConfig.getMaxRetryBackoffMs()).map(Long::valueOf).ifPresent(config::setMaxRetryBackoffMs);

        threadExecutor = createThreadPoolExecutor(tencentClsLogConfig.getSendThreadCount());

        try {
            client = new AsyncProducerClient(config);
        } catch (Exception e) {
            LOG.warn("TencentClsLogCollectClient initClient error message:{}", e.getMessage());
        }
    }

    /**
     * Tencent cls consume.
     *
     * @param logs list of log
     */
    @Override
    public void consume0(@NonNull final List<ShenyuRequestLog> logs) {
        logs.forEach(this::sendLog);
    }

    @Override
    public void close0() {
        if (Objects.nonNull(client)) {
            try {
                client.close();
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
        LogItem logItem = new LogItem((int) (System.currentTimeMillis() / 1000));
        logItem.PushBack("level", "info");
        logItem.PushBack("name", log.getRequestUri());
        logItem.PushBack("message", GsonUtils.getGson().toJson(log));
        logItems.add(logItem);
        try {
            final ListenableFuture<Result> f = client.putLogs(topic, logItems, result -> { });
            Futures.addCallback(f, new ProducerFutureCallback(topic), threadExecutor);
        } catch (InterruptedException e) {
            LOG.warn("The current thread has been interrupted during send logs.");
        } catch (Exception e) {
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
                new LinkedBlockingQueue<>(GenericLoggingConstant.MAX_QUEUE_NUMBER), ShenyuThreadFactory.create("shenyu-tencent-cls", true),
                new ThreadPoolExecutor.AbortPolicy());
    }

    /**
     * Producer Future Callback.
     */
    private static final class ProducerFutureCallback implements FutureCallback<Result> {

        private static final Logger LOGGER = LoggerFactory.getLogger(ProducerFutureCallback.class);

        private final String topic;

        ProducerFutureCallback(final String topic) {
            this.topic = topic;
        }

        @Override
        public void onSuccess(@Nullable final Result result) {
            LOGGER.info("Send logs to Tencent cls successfully.");
        }

        @Override
        public void onFailure(final Throwable throwable) {
            if (throwable instanceof ResultFailedException) {
                Result result = ((ResultFailedException) throwable).getResult();
                LOGGER.error("Failed to send logs, topic={}, result={}", topic, result);
            } else {
                LOGGER.error("Failed to send log, e={}", throwable.getMessage());
            }
        }
    }
}
