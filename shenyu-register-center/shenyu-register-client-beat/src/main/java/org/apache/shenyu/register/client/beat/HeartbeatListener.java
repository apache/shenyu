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

package org.apache.shenyu.register.client.beat;

import com.github.benmanes.caffeine.cache.CacheLoader;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.util.concurrent.MoreExecutors;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.utils.AesUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.utils.RegisterUtils;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.EventListener;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

public class HeartbeatListener {

    private static final Logger LOG = LoggerFactory.getLogger(HeartbeatListener.class);

    private static final int INITIAL_DELAY = 0;

    private static final int CHECK_PERIOD = 5;

    private static final int MAX_BEAT_THREADS = 8;

    private static final String HEARTBEAT_TIMEOUT_MS = "heartbeatTimeoutMs";

    private static final String HEARTBEAT_DIRECT_EXECUTION = "heartbeatDirectExecution";

    private static final long DEFAULT_DEADLINE_MS = 4000L;

    private final boolean directExecution;

    private ScheduledThreadPoolExecutor executor;

    private ExecutorService beatExecutor;

    private BeatSender beatSender;

    private final long deadlineMs;

    private final Map<String, AtomicBoolean> inFlight = new ConcurrentHashMap<>();

    private final ShenyuConfig shenyuConfig;

    private String username;

    private String password;

    private List<String> serverList;

    /**
     * server -> accessToken.
     */
    private LoadingCache<String, String> accessToken;

    private ShenyuBootstrapHeartBeatConfig config;

    public HeartbeatListener(final ShenyuBootstrapHeartBeatConfig config, final ShenyuConfig shenyuConfig, final ServerProperties serverProperties) {
        this(config, shenyuConfig, serverProperties, null, null);
    }

    public HeartbeatListener(final ShenyuBootstrapHeartBeatConfig config,
                             final ShenyuConfig shenyuConfig,
                             final ServerProperties serverProperties,
                             final ExecutorService beatExecutor,
                             final BeatSender beatSender) {
        this.executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-instance-task", false));
        this.shenyuConfig = shenyuConfig;
        LOG.info("Web server initialized on port {}, starting heartbeat reporter", serverProperties.getPort());
        this.username = config.getProps().getProperty(Constants.USER_NAME);
        this.password = config.getProps().getProperty(Constants.PASS_WORD);
        this.config = config;
        String secretKey = config.getProps().getProperty(Constants.AES_SECRET_KEY);
        String secretIv = config.getProps().getProperty(Constants.AES_SECRET_IV);
        if (StringUtils.isNotBlank(secretKey) && StringUtils.isNotBlank(secretIv)) {
            this.password = AesUtils.cbcEncrypt(secretKey, secretIv, password);
        }
        this.serverList = Lists.newArrayList(Splitter.on(",").split(config.getServerLists()));
        String timeoutProp = config.getProps().getProperty(HEARTBEAT_TIMEOUT_MS);
        this.deadlineMs = StringUtils.isNotBlank(timeoutProp) ? Long.parseLong(timeoutProp) : DEFAULT_DEADLINE_MS;
        String directProp = config.getProps().getProperty(HEARTBEAT_DIRECT_EXECUTION);
        this.directExecution = Boolean.parseBoolean(directProp);
        if (Objects.nonNull(beatExecutor)) {
            this.beatExecutor = beatExecutor;
        } else if (this.directExecution) {
            this.beatExecutor = MoreExecutors.newDirectExecutorService();
        } else {
            int poolSize = Math.min(Math.max(serverList.size(), 1), MAX_BEAT_THREADS);
            this.beatExecutor = new ThreadPoolExecutor(poolSize, poolSize,
                    60L, TimeUnit.SECONDS,
                    new ArrayBlockingQueue<>(poolSize * 2),
                    ShenyuThreadFactory.create("instance-beat-worker", true),
                    new ThreadPoolExecutor.DiscardPolicy());
        }
        this.beatSender = Optional.ofNullable(beatSender).orElseGet(DefaultBeatSender::new);
        this.accessToken = Caffeine.newBuilder()
                //see org.apache.shenyu.admin.config.properties.JwtProperties#expiredSeconds
                .expireAfterWrite(24L, TimeUnit.HOURS)
                .build(new CacheLoader<>() {
                    @Override
                    public @Nullable String load(@NonNull final String server) {
                        try {
                            Optional<Object> login = HeartbeatListener.this.beatSender.login(username, password, server.concat(Constants.LOGIN_PATH));
                            return login.map(String::valueOf).orElse(null);
                        } catch (Exception e) {
                            LOG.error("Login admin url :{} is fail, will retry. cause: {} ", server, e.getMessage());
                            return null;
                        }
                    }
                });
        executor.scheduleAtFixedRate(() -> {
            InstanceBeatInfoDTO instanceBeatInfoDTO = new InstanceBeatInfoDTO();
            instanceBeatInfoDTO.setInstancePort(String.valueOf(serverProperties.getPort()));
            instanceBeatInfoDTO.setInstanceIp(IpUtils.getHost());
            instanceBeatInfoDTO.setNamespaceId(shenyuConfig.getNamespace());
            instanceBeatInfoDTO.setInstanceInfo(SystemInfoUtils.getSystemInfo());
            instanceBeatInfoDTO.setInstanceType(InstanceTypeConstants.BOOTSTRAP_INSTANCE_INFO);
            sendHeartbeat(instanceBeatInfoDTO);
            }, INITIAL_DELAY, CHECK_PERIOD, TimeUnit.SECONDS
        );
    }

    private void sendHeartbeat(final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        for (String server : serverList) {
            AtomicBoolean serverInFlight = null;
            if (!directExecution) {
                serverInFlight = inFlight.computeIfAbsent(server, k -> new AtomicBoolean(false));
                if (!serverInFlight.compareAndSet(false, true)) {
                    LOG.warn("Previous heartbeat to admin url :{} is still in flight, skipping.", server);
                    continue;
                }
            }
            final AtomicBoolean finalInFlight = serverInFlight;
            Future<?> future = beatExecutor.submit(() -> {
                try {
                    sendHeartbeatToServer(server, instanceBeatInfoDTO);
                } finally {
                    if (Objects.nonNull(finalInFlight)) {
                        finalInFlight.set(false);
                    }
                }
            });
            if (deadlineMs > 0 && Objects.nonNull(executor) && !executor.isShutdown()) {
                try {
                    executor.schedule(() -> {
                        if (!future.isDone()) {
                            future.cancel(true);
                            LOG.warn("Heartbeat to admin url :{} timed out after {} ms deadline.", server, deadlineMs);
                        }
                    }, deadlineMs, TimeUnit.MILLISECONDS);
                } catch (RejectedExecutionException e) {
                    LOG.warn("Scheduler rejected timeout task for admin url: {}", server);
                }
            }
        }
    }

    private void sendHeartbeatToServer(final String server, final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        String concat = server.concat(Constants.BEAT_URI_PATH);
        try {
            String token = this.accessToken.get(server);
            if (StringUtils.isBlank(token)) {
                throw new NullPointerException("accessToken is null");
            }
            beatSender.doHeartBeat(GsonUtils.getInstance().toJson(instanceBeatInfoDTO), concat, Constants.HEARTBEAT, token);
        } catch (Exception e) {
            LOG.error("HeartBeat admin url :{} is fail, will retry.", server, e);
            // This is a periodic reporter, not a failback registration: retry on the next tick.
        }
    }

    @EventListener(ContextClosedEvent.class)
    public void onShutdown() {
        if (Objects.nonNull(executor)) {
            executor.shutdown();
        }
        if (Objects.nonNull(beatExecutor)) {
            beatExecutor.shutdown();
        }
        try {
            if (Objects.nonNull(executor) && !executor.awaitTermination(5, TimeUnit.SECONDS)) {
                executor.shutdownNow();
            }
            if (Objects.nonNull(beatExecutor) && !beatExecutor.awaitTermination(5, TimeUnit.SECONDS)) {
                beatExecutor.shutdownNow();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    void setBeatExecutor(final ExecutorService beatExecutor) {
        this.beatExecutor = beatExecutor;
    }

    void setBeatSender(final BeatSender beatSender) {
        this.beatSender = beatSender;
    }

    ExecutorService getBeatExecutor() {
        return beatExecutor;
    }

    BeatSender getBeatSender() {
        return beatSender;
    }

    public interface BeatSender {

        /**
         * Login to admin.
         *
         * @param username username
         * @param password password
         * @param url url
         * @return login result
         * @throws Exception exception
         */
        Optional<Object> login(String username, String password, String url) throws Exception;

        /**
         * Send heartbeat to admin.
         *
         * @param json json payload
         * @param url url
         * @param type type
         * @param token token
         * @throws Exception exception
         */
        void doHeartBeat(String json, String url, String type, String token) throws Exception;
    }

    public static class DefaultBeatSender implements BeatSender {

        @Override
        public Optional<Object> login(final String username, final String password, final String url) throws Exception {
            return RegisterUtils.doLogin(username, password, url);
        }

        @Override
        public void doHeartBeat(final String json, final String url, final String type, final String token) throws Exception {
            RegisterUtils.doHeartBeat(json, url, type, token);
        }
    }
}
