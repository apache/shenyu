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

package org.apache.shenyu.agent.plugin.logging.common;

import org.apache.shenyu.agent.plugin.logging.LogCollector;
import org.apache.shenyu.agent.plugin.logging.spi.LogCollectClient;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.ThreadUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
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

    private final ExecutorService threadExecutor = Executors.newSingleThreadExecutor();

    private final int bufferSize = 50000;

    private final int batchSize = 100;

    private final int diffTimeMSForPush = 100;

    private final BlockingQueue<String> bufferQueue = new LinkedBlockingDeque<>(bufferSize);

    private long lastPushTime;

    private final AtomicBoolean started = new AtomicBoolean(true);
    
    private final LogCollectClient logCollectClient;

    public AbstractLogCollector(final LogCollectClient logCollectClient) {
        this.logCollectClient = logCollectClient;
        threadExecutor.execute(this::consume);
    }

    @Override
    public void collect(final Object log) {
        if (log == null) {
            return;
        }
        if (bufferQueue.size() < bufferSize) {
            if (log instanceof String) {
                bufferQueue.add((String) log);
            } else {
                bufferQueue.add(JsonUtils.toJson(log));
            }
        }
    }

    /**
     * batch and async consume.
     */
    private void consume() {
        while (started.get()) {
            try {
                List<String> logs = new ArrayList<>();
                int size = bufferQueue.size();
                long time = System.currentTimeMillis();
                long timeDiffMs = time - lastPushTime;
                if (size >= batchSize || timeDiffMs > diffTimeMSForPush) {
                    bufferQueue.drainTo(logs, batchSize);
                    logCollectClient.collect(logs);
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

    @Override
    public void close() throws Exception {
        started.set(false);
        logCollectClient.close();
    }
    
}
