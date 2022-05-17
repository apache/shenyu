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

package org.apache.shenyu.plugin.logging.rocketmq;

import org.apache.shenyu.common.utils.ThreadUtils;
import org.apache.shenyu.plugin.logging.rocketmq.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.rocketmq.utils.LogCollectConfigUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * abstract log collector,Contains common methods.
 */
public abstract class AbstractLogCollector implements LogCollector {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractLogCollector.class);

    private int bufferSize;

    private BlockingQueue<ShenyuRequestLog> bufferQueue;

    private long lastPushTime;

    private final AtomicBoolean started = new AtomicBoolean(true);

    @Override
    public void start() {
        bufferSize = LogCollectConfigUtils.getGlobalLogConfig().getBufferQueueSize();
        bufferQueue = new LinkedBlockingDeque<>(bufferSize);
        ExecutorService threadExecutor = Executors.newSingleThreadExecutor();
        started.set(true);
        threadExecutor.execute(this::consume);
    }

    @Override
    public void collect(final ShenyuRequestLog log) {
        if (Objects.isNull(log) || Objects.isNull(getLogConsumeClient())) {
            return;
        }
        if (bufferQueue.size() < bufferSize) {
            bufferQueue.add(log);
        }
    }

    /**
     * batch and async consume.
     */
    private void consume() {
        while (started.get()) {
            int diffTimeMSForPush = 100;
            try {
                List<ShenyuRequestLog> logs = new ArrayList<>();
                int size = bufferQueue.size();
                long time = System.currentTimeMillis();
                long timeDiffMs = time - lastPushTime;
                int batchSize = 100;
                if (size >= batchSize || timeDiffMs > diffTimeMSForPush) {
                    bufferQueue.drainTo(logs, batchSize);
                    LogConsumeClient logCollectClient = getLogConsumeClient();
                    if (logCollectClient != null) {
                        logCollectClient.consume(logs);
                    }
                    lastPushTime = time;
                } else {
                    ThreadUtils.sleep(TimeUnit.MILLISECONDS, diffTimeMSForPush);
                }
            } catch (Exception e) {
                LOG.error("DefaultLogCollector collect log error", e);
                ThreadUtils.sleep(TimeUnit.MILLISECONDS, diffTimeMSForPush);
            }
        }
    }

    /**
     * get log consume client.
     *
     * @return log consume client
     */
    protected abstract LogConsumeClient getLogConsumeClient();

    @Override
    public void close() throws Exception {
        started.set(false);
        LogConsumeClient logCollectClient = getLogConsumeClient();
        if (logCollectClient != null) {
            logCollectClient.close();
        }
    }
}
