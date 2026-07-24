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

package org.apache.shenyu.plugin.record.collector;

import com.github.benmanes.caffeine.cache.CacheLoader;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import okhttp3.Headers;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.MemorySafeTaskQueue;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.common.utils.ThreadUtils;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.record.config.HttpRecordCollectConfig;
import org.apache.shenyu.plugin.record.config.ShenyuHttpRecordCenterConfig;
import org.apache.shenyu.plugin.record.entity.ShenyuHttpRequestRecord;
import org.apache.shenyu.plugin.record.utils.OkHttpTools;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.CRC32;

/**
 * HttpRecordCollector.
 *
 * <p>Collects recorded HTTP request/response data from the gateway plugin,
 * buffers them in a lock-free queue, and uploads batches to the admin server.</p>
 */
public class HttpRecordCollector {

    public static final HttpRecordCollector INSTANCE = new HttpRecordCollector();

    private static final Logger LOG = LoggerFactory.getLogger(HttpRecordCollector.class);

    private static final long ACCESS_TOKEN_CACHE_TTL_HOURS = 24L;

    private static final int CONSUMER_POLL_INTERVAL_MS = 50;

    private static final int CONSUMER_ERROR_BACKOFF_MS = 100;

    private static final int SHUTDOWN_AWAIT_TIMEOUT_SECONDS = 30;

    private final AtomicBoolean started = new AtomicBoolean(false);

    private int maxBufferSize;

    private final AtomicInteger bufferSize = new AtomicInteger(0);

    private ConcurrentLinkedQueue<ShenyuHttpRequestRecord> bufferQueue;

    private ShenyuHttpRecordCenterConfig shenyuHttpRecordCenterConfig;

    private LoadingCache<String, String> accessToken;

    private HttpRecordCollectConfig.RecordConfig config;

    private String username;

    private String password;

    private List<String> serverList;

    private ShenyuThreadPoolExecutor threadExecutor;

    /**
     * Start the collector: initialize config, login cache, buffer queue and consumer thread.
     */
    public void start() {
        if (!started.compareAndSet(false, true)) {
            return;
        }
        shenyuHttpRecordCenterConfig = SpringBeanUtils.getInstance().getBean(ShenyuHttpRecordCenterConfig.class);
        this.username = shenyuHttpRecordCenterConfig.getProps().getProperty(Constants.USER_NAME);
        this.password = shenyuHttpRecordCenterConfig.getProps().getProperty(Constants.PASS_WORD);
        this.serverList = Lists.newArrayList(Splitter.on(",").split(shenyuHttpRecordCenterConfig.getServerLists()));
        config = HttpRecordCollectConfig.INSTANCE.getRecordConfig();
        this.accessToken = Caffeine.newBuilder()
                .expireAfterWrite(ACCESS_TOKEN_CACHE_TTL_HOURS, TimeUnit.HOURS)
                .build(new CacheLoader<>() {
                    @Override
                    public @Nullable String load(@NonNull final String server) {
                        try {
                            Optional<?> login = doLogin(username, password, server.concat(Constants.LOGIN_PATH));
                            return login.map(String::valueOf).orElse(null);
                        } catch (Exception e) {
                            LOG.error("Login admin url :{} is fail, will retry. cause: {} ", server, e.getMessage());
                            return null;
                        }
                    }
                });
        maxBufferSize = Math.max(config.getBatchSize() * 500, 5000);
        bufferQueue = new ConcurrentLinkedQueue<>();
        ShenyuConfig shenyuConfig = Optional.ofNullable(Singleton.INST.get(ShenyuConfig.class)).orElse(new ShenyuConfig());
        final ShenyuConfig.SharedPool sharedPool = shenyuConfig.getSharedPool();
        threadExecutor = new ShenyuThreadPoolExecutor(sharedPool.getCorePoolSize(),
                sharedPool.getMaximumPoolSize(), sharedPool.getKeepAliveTime(), TimeUnit.MILLISECONDS,
                new MemorySafeTaskQueue<>(Constants.THE_256_MB),
                ShenyuThreadFactory.create(shenyuConfig.getSharedPool().getPrefix(), true),
                new ThreadPoolExecutor.AbortPolicy());
        threadExecutor.execute(this::consume);
    }

    /**
     * Stop the collector: signal the consumer thread to stop and shutdown the thread pool.
     */
    public void stop() {
        if (!started.compareAndSet(true, false)) {
            return;
        }
        if (Objects.nonNull(threadExecutor)) {
            threadExecutor.shutdown();
            try {
                if (!threadExecutor.awaitTermination(SHUTDOWN_AWAIT_TIMEOUT_SECONDS, TimeUnit.SECONDS)) {
                    LOG.warn("Thread executor did not terminate within {} seconds", SHUTDOWN_AWAIT_TIMEOUT_SECONDS);
                    threadExecutor.shutdownNow();
                }
            } catch (InterruptedException e) {
                threadExecutor.shutdownNow();
                Thread.currentThread().interrupt();
            }
        }
        if (Objects.nonNull(accessToken)) {
            accessToken.cleanUp();
        }
    }

    /**
     * Collect a record into the buffer queue. This method is lock-free and safe to call from
     * the WebFlux Event Loop thread.
     *
     * @param record the http request record to collect
     */
    public void collect(final ShenyuHttpRequestRecord record) {
        if (Objects.isNull(record) || Objects.isNull(bufferQueue)) {
            return;
        }
        if (bufferSize.get() < maxBufferSize) {
            bufferSize.incrementAndGet();
            bufferQueue.offer(record);
        } else {
            LOG.warn("Record buffer is full, dropping record. traceId={}", record.getTraceId());
        }
    }

    private void consume() {
        int batchSize = config.getBatchSize();
        long batchIntervalMs = config.getBatchIntervalMs();
        List<ShenyuHttpRequestRecord> batch = new ArrayList<>();
        long lastFlushTime = System.currentTimeMillis();
        while (started.get()) {
            try {
                ShenyuHttpRequestRecord record = bufferQueue.poll();
                if (Objects.nonNull(record)) {
                    bufferSize.decrementAndGet();
                    batch.add(record);
                }
                boolean batchFull = batch.size() >= batchSize;
                boolean timeout = !batch.isEmpty() && System.currentTimeMillis() - lastFlushTime >= batchIntervalMs;
                if (batchFull || timeout) {
                    flushBatch(batch);
                    batch.clear();
                    lastFlushTime = System.currentTimeMillis();
                } else if (bufferQueue.isEmpty()) {
                    ThreadUtils.sleep(TimeUnit.MILLISECONDS, CONSUMER_POLL_INTERVAL_MS);
                }
            } catch (Throwable t) {
                LOG.error("ShenyuHttpRequestRecord collect log error", t);
                ThreadUtils.sleep(TimeUnit.MILLISECONDS, CONSUMER_ERROR_BACKOFF_MS);
            }
        }
        if (!batch.isEmpty()) {
            try {
                flushBatch(batch);
            } catch (Exception e) {
                LOG.error("Failed to flush remaining records on shutdown", e);
            }
        }
    }

    private void flushBatch(final List<ShenyuHttpRequestRecord> records) throws IOException {
        String jsonPayload = GsonUtils.getInstance().toJson(records);
        byte[] payloadBytes = jsonPayload.getBytes(StandardCharsets.UTF_8);
        CRC32 crc32 = new CRC32();
        crc32.update(payloadBytes);
        long checkSum = crc32.getValue();
        doUploadRecord(records, checkSum);
    }

    private void doUploadRecord(final List<ShenyuHttpRequestRecord> records, final long checkSum) throws IOException {
        for (String server : serverList) {
            String contact = server.concat(Constants.RECORD_UPLOAD_PATH);
            String token = this.accessToken.get(server);
            if (StringUtils.isBlank(token)) {
                throw new IllegalStateException("accessToken is blank, login may have failed for server: " + server);
            }
            Headers headers = new Headers.Builder().add(Constants.X_ACCESS_TOKEN, token)
                    .add(Constants.X_RECORD_CRC32, String.valueOf(checkSum))
                    .build();
            OkHttpTools.getInstance().post(contact, GsonUtils.getInstance().toJson(records), headers);
        }
    }

    /**
     * Gets instance.
     *
     * @return the singleton instance
     */
    public static HttpRecordCollector getInstance() {
        return INSTANCE;
    }

    private static Optional<Object> doLogin(final String username, final String password,
                                            final String url) throws IOException {
        Map<String, Object> loginMap = new HashMap<>(2);
        loginMap.put(Constants.LOGIN_NAME, username);
        loginMap.put(Constants.PASS_WORD, password);
        String result = OkHttpTools.getInstance().get(url, loginMap);
        Map<String, Object> resultMap = GsonUtils.getInstance().convertToMap(result);
        if (!String.valueOf(CommonErrorCode.SUCCESSFUL).equals(String.valueOf(resultMap.get(Constants.ADMIN_RESULT_CODE)))) {
            return Optional.empty();
        }
        String tokenJson = GsonUtils.getInstance().toJson(resultMap.get(Constants.ADMIN_RESULT_DATA));
        LOG.info("login success: {} ", tokenJson);
        Map<String, Object> tokenMap = GsonUtils.getInstance().convertToMap(tokenJson);
        return Optional.ofNullable(tokenMap.get(Constants.ADMIN_RESULT_TOKEN));
    }

}
