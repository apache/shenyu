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

package org.apache.shenyu.plugin.aliyun.sls.client;

import com.aliyun.openservices.aliyun.log.producer.LogProducer;
import com.aliyun.openservices.aliyun.log.producer.Producer;
import com.aliyun.openservices.aliyun.log.producer.ProducerConfig;
import com.aliyun.openservices.aliyun.log.producer.ProjectConfig;
import com.aliyun.openservices.aliyun.log.producer.Result;
import com.aliyun.openservices.aliyun.log.producer.errors.LogSizeTooLargeException;
import com.aliyun.openservices.aliyun.log.producer.errors.MaxBatchCountExceedException;
import com.aliyun.openservices.aliyun.log.producer.errors.ProducerException;
import com.aliyun.openservices.aliyun.log.producer.errors.ResultFailedException;
import com.aliyun.openservices.log.Client;
import com.aliyun.openservices.log.common.LogItem;
import com.aliyun.openservices.log.common.LogStore;
import com.aliyun.openservices.log.exception.LogException;
import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.client.LogConsumeClient;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Aliyun sls log Collect client.
 */
public class AliyunSlsLogCollectClient implements LogConsumeClient {

    private static final Logger LOG = LoggerFactory.getLogger(AliyunSlsLogCollectClient.class);

    private Client client;

    private String projectName;

    private String logStore;

    private String topic;

    private Producer producer;

    private final AtomicBoolean isStarted = new AtomicBoolean(false);

    private ThreadPoolExecutor threadExecutor;

    /**
     * init aliyun sls client.
     *
     * @param props props
     */
    public void initClient(final Properties props) {
        if (MapUtils.isEmpty(props)) {
            LOG.error("aliyun sls props is empty. failed init aliyun sls producer");
            return;
        }
        if (isStarted.get()) {
            close();
        }
        String accessId = props.getProperty(GenericLoggingConstant.ACCESS_ID);
        String accessKey = props.getProperty(GenericLoggingConstant.ACCESS_KEY);
        String host = props.getProperty(GenericLoggingConstant.HOST);
        if (StringUtils.isBlank(accessId) || StringUtils.isBlank(accessKey) || StringUtils.isBlank(host)) {
            LOG.error("init aliyun sls client error, please check accessId, accessKey or host");
            return;
        }
        client = new Client(host, accessId, accessKey);
        // create LogStore, if you don't create logStore, shenyu will do it.
        projectName = props.getProperty(GenericLoggingConstant.PROJECT_NAME);
        topic = props.getProperty(GenericLoggingConstant.TOPIC);
        logStore = props.getProperty(GenericLoggingConstant.LOG_STORE);
        int ttlInDay = Integer.parseInt(props.getProperty(GenericLoggingConstant.TTL_IN_DAY));
        int shardCount = Integer.parseInt(props.getProperty(GenericLoggingConstant.SHARD_COUNT));
        // init projectConfig, producer, logStore
        ProjectConfig projectConfig = new ProjectConfig(projectName, host, accessId, accessKey);
        producer = createProducer(props, projectConfig);
        LogStore store = new LogStore(logStore, ttlInDay, shardCount);
        threadExecutor = createThreadPoolExecutor(props);
        try {
            isStarted.set(true);
            Runtime.getRuntime().addShutdownHook(new Thread(this::close));
            client.CreateLogStore(projectName, store);
        } catch (LogException e) {
            LOG.warn("error code:{}, error message:{}", e.GetErrorCode(), e.GetErrorMessage());
        }
    }

    /**
     * aliyun sls consume.
     *
     * @param logs list of log
     */
    @Override
    public void consume(final List<ShenyuRequestLog> logs) {
        if (CollectionUtils.isEmpty(logs) || !isStarted.get()) {
            return;
        }
        logs.forEach(this::sendLog);
    }

    @Override
    public void close() {
        if (Objects.nonNull(client) && isStarted.get()) {
            isStarted.set(false);
            client.shutdown();
            try {
                producer.close();
            } catch (InterruptedException | ProducerException e) {
                LOG.error("Close producer error.");
            }
        }
    }

    /**
     * send log to aliyun sls.
     *
     * @param log log
     */
    private void sendLog(final ShenyuRequestLog log) {
        final List<LogItem> logGroup = new ArrayList<>();
        LogItem logItem = new LogItem((int) (System.currentTimeMillis() / 1000));
        logItem.PushBack("level", "info");
        logItem.PushBack("name", log.getRequestUri());
        logItem.PushBack("message", GsonUtils.getGson().toJson(log));
        logGroup.add(logItem);
        try {
            ListenableFuture<Result> f = producer.send(projectName, logStore, topic, GenericLoggingConstant.DEFAULT_SOURCE, logGroup);
            Futures.addCallback(f, new ProducerFutureCallback(projectName, logStore), threadExecutor);
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
     * create aliyun sls producer.
     *
     * @param props         props
     * @param projectConfig project config
     * @return {@linkplain Producer}
     */
    private static Producer createProducer(final Properties props, final ProjectConfig projectConfig) {
        int ioThreadCount = Integer.parseInt(props.getProperty(GenericLoggingConstant.IO_THREAD_COUNT));
        if (ioThreadCount > GenericLoggingConstant.MAX_ALLOW_THREADS) {
            LOG.warn("io thread count number too large!");
            ioThreadCount = GenericLoggingConstant.MAX_ALLOW_THREADS;
        }
        ProducerConfig producerConfig = new ProducerConfig();
        producerConfig.setIoThreadCount(ioThreadCount);
        producerConfig.setLogFormat(ProducerConfig.LogFormat.JSON);
        Producer producer = new LogProducer(producerConfig);
        producer.putProjectConfig(projectConfig);
        return producer;
    }

    /**
     * create send log queue.
     *
     * @param props props
     * @return ThreadPoolExecutor
     */
    private static ThreadPoolExecutor createThreadPoolExecutor(final Properties props) {
        int sendThreadCount = Integer.parseInt(props.getProperty(GenericLoggingConstant.SEND_THREAD_COUNT));
        if (sendThreadCount > GenericLoggingConstant.MAX_ALLOW_THREADS) {
            LOG.warn("send thread count number too large!");
            sendThreadCount = GenericLoggingConstant.MAX_ALLOW_THREADS;
        }
        return new ThreadPoolExecutor(sendThreadCount, GenericLoggingConstant.MAX_ALLOW_THREADS, 60000L, TimeUnit.MICROSECONDS,
                new LinkedBlockingQueue<>(GenericLoggingConstant.MAX_QUEUE_NUMBER), ShenyuThreadFactory.create("shenyu-aliyun-sls", true),
                new ThreadPoolExecutor.AbortPolicy());
    }

    /**
     * Producer Future Callback.
     */
    private static final class ProducerFutureCallback implements FutureCallback<Result> {

        private static final Logger LOGGER = LoggerFactory.getLogger(ProducerFutureCallback.class);

        private final String project;

        private final String logStore;

        ProducerFutureCallback(final String project, final String logStore) {
            this.project = project;
            this.logStore = logStore;
        }

        @Override
        public void onSuccess(@Nullable final Result result) {
            LOGGER.info("Send logs to aliyun sls successfully.");
        }

        @Override
        public void onFailure(final Throwable throwable) {
            if (throwable instanceof ResultFailedException) {
                Result result = ((ResultFailedException) throwable).getResult();
                LOGGER.error("Failed to send logs, project={}, logStore={}, result={}", project, logStore, result);
            } else {
                LOGGER.error("Failed to send log, e={}", throwable.getMessage());
            }
        }
    }
}
