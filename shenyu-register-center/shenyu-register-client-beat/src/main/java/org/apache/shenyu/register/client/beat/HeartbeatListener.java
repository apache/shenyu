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
import org.apache.shenyu.common.utils.BoundServerPort;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.exception.HttpResponseException;
import org.apache.shenyu.register.common.utils.RegisterUtils;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.EventListener;

import java.net.HttpURLConnection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.LongSupplier;

public class HeartbeatListener {

    private static final Logger LOG = LoggerFactory.getLogger(HeartbeatListener.class);

    private static final int INITIAL_DELAY = 0;

    private static final int CHECK_PERIOD = 5;

    private static final int MAX_BEAT_THREADS = 8;

    private static final long MAX_LOGIN_RETRY_DELAY_SECONDS = 60L;

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

    private final LongSupplier nanoTime;

    // Heartbeat workers update retry state independently for each Admin.
    private final Map<String, LoginRetry> loginRetries = new ConcurrentHashMap<>();

    private String username;

    private String password;

    private List<String> serverList;

    /**
     * server -> accessToken.
     */
    private LoadingCache<String, String> accessToken;

    private ShenyuBootstrapHeartBeatConfig config;

    /**
     * Port of the application listener. Zero until {@link WebServerInitializedEvent}.
     */
    private volatile int boundPort;

    private volatile boolean closed;

    public HeartbeatListener(final ShenyuBootstrapHeartBeatConfig config, final ShenyuConfig shenyuConfig) {
        this(config, shenyuConfig, null, null, System::nanoTime);
    }

    HeartbeatListener(final ShenyuBootstrapHeartBeatConfig config, final ShenyuConfig shenyuConfig, final LongSupplier nanoTime) {
        this(config, shenyuConfig, null, null, nanoTime);
    }

    public HeartbeatListener(final ShenyuBootstrapHeartBeatConfig config,
                             final ShenyuConfig shenyuConfig,
                             final ExecutorService beatExecutor,
                             final BeatSender beatSender) {
        this(config, shenyuConfig, beatExecutor, beatSender, System::nanoTime);
    }

    HeartbeatListener(final ShenyuBootstrapHeartBeatConfig config,
                      final ShenyuConfig shenyuConfig,
                      final ExecutorService beatExecutor,
                      final BeatSender beatSender,
                      final LongSupplier nanoTime) {
        this.executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-instance-task", false));
        this.shenyuConfig = shenyuConfig;
        this.nanoTime = nanoTime;
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
        this.deadlineMs = parseDeadlineMs(timeoutProp);
        String directProp = config.getProps().getProperty(HEARTBEAT_DIRECT_EXECUTION);
        this.directExecution = Boolean.parseBoolean(directProp);
        if (Objects.nonNull(beatExecutor)) {
            this.beatExecutor = beatExecutor;
        } else if (this.directExecution) {
            this.beatExecutor = MoreExecutors.newDirectExecutorService();
        } else {
            int poolSize = Math.min(Math.max(1, this.serverList.size()), MAX_BEAT_THREADS);
            this.beatExecutor = new ThreadPoolExecutor(
                    poolSize, poolSize,
                    60L, TimeUnit.SECONDS,
                    new ArrayBlockingQueue<>(this.serverList.size()),
                    ShenyuThreadFactory.create("instance-beat-worker", false),
                    new ThreadPoolExecutor.AbortPolicy());
        }
        this.beatSender = Optional.ofNullable(beatSender).orElseGet(DefaultBeatSender::new);
        this.accessToken = Caffeine.newBuilder()
                //see org.apache.shenyu.admin.config.properties.JwtProperties#expiredSeconds
                .expireAfterWrite(24L, TimeUnit.HOURS)
                .build(new CacheLoader<>() {
                    @Override
                    public @Nullable String load(@NonNull final String server) {
                        try {
                            Optional<?> login = HeartbeatListener.this.beatSender.login(username, password, server.concat(Constants.LOGIN_PATH));
                            String token = login.map(String::valueOf).orElse(null);
                            if (StringUtils.isBlank(token)) {
                                deferLogin(server);
                                LOG.warn("Login admin url :{} returned no access token, will retry with backoff.", server);
                                return null;
                            }
                            return token;
                        } catch (Exception e) {
                            deferLogin(server);
                            LOG.error("Login admin url :{} is fail, will retry. cause: {} ", server, e.getMessage());
                            return null;
                        }
                    }
                });
        executor.scheduleAtFixedRate(this::reportHeartbeat, INITIAL_DELAY, CHECK_PERIOD, TimeUnit.SECONDS);
    }

    /**
     * Parse the heartbeat timeout deadline in milliseconds.
     * Values &lt;= 0 (e.g. -1) disable the per-destination deadline entirely.
     *
     * @param timeoutProp timeout property string
     * @return deadline in milliseconds, or &lt;= 0 if disabled
     */
    private long parseDeadlineMs(final String timeoutProp) {
        if (StringUtils.isBlank(timeoutProp)) {
            return DEFAULT_DEADLINE_MS;
        }
        try {
            return Long.parseLong(timeoutProp.trim());
        } catch (NumberFormatException e) {
            LOG.warn("Invalid heartbeatTimeoutMs property value '{}', falling back to default {} ms", timeoutProp, DEFAULT_DEADLINE_MS);
            return DEFAULT_DEADLINE_MS;
        }
    }

    /**
     * Record the application listener port. The management server is ignored, and nothing is reported
     * until a positive bound port is observed. Shutdown wins over a late initialization event.
     *
     * @param event web server initialization event
     */
    @EventListener
    public void onWebServerInitialized(final WebServerInitializedEvent event) {
        if (closed || Objects.isNull(event.getWebServer()) || Objects.isNull(event.getApplicationContext())) {
            return;
        }
        OptionalInt resolved = BoundServerPort.resolve(event.getApplicationContext().getServerNamespace(), event.getWebServer().getPort());
        if (resolved.isEmpty()) {
            LOG.debug("Ignore heartbeat port from namespace {} port {}", event.getApplicationContext().getServerNamespace(), event.getWebServer().getPort());
            return;
        }
        int observed = resolved.getAsInt();
        if (boundPort == observed) {
            return;
        }
        boundPort = observed;
        LOG.info("Application server bound on port {}, heartbeat reporter will use it", observed);
    }

    private void reportHeartbeat() {
        if (closed) {
            return;
        }
        int port = boundPort;
        if (port <= 0) {
            return;
        }
        InstanceBeatInfoDTO instanceBeatInfoDTO = new InstanceBeatInfoDTO();
        instanceBeatInfoDTO.setInstancePort(String.valueOf(port));
        instanceBeatInfoDTO.setInstanceIp(IpUtils.getHost());
        instanceBeatInfoDTO.setNamespaceId(shenyuConfig.getNamespace());
        instanceBeatInfoDTO.setInstanceInfo(SystemInfoUtils.getSystemInfo());
        instanceBeatInfoDTO.setInstanceType(InstanceTypeConstants.BOOTSTRAP_INSTANCE_TYPE);
        sendHeartbeat(instanceBeatInfoDTO);
    }

    private void sendHeartbeat(final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        for (String server : serverList) {
            AtomicBoolean serverInFlight = null;
            if (!directExecution) {
                serverInFlight = inFlight.computeIfAbsent(server, k -> new AtomicBoolean(false));
                if (!serverInFlight.compareAndSet(false, true)) {
                    LOG.debug("Previous heartbeat to admin url :{} is still in flight, skipping.", server);
                    continue;
                }
            }
            final AtomicBoolean finalInFlight = serverInFlight;
            Future<?> future;
            try {
                future = beatExecutor.submit(() -> {
                    try {
                        sendHeartbeatToServer(server, instanceBeatInfoDTO);
                    } finally {
                        if (Objects.nonNull(finalInFlight)) {
                            finalInFlight.set(false);
                        }
                    }
                });
            } catch (RejectedExecutionException e) {
                if (Objects.nonNull(finalInFlight)) {
                    finalInFlight.set(false);
                }
                LOG.warn("Heartbeat dispatch rejected for admin url: {}", server, e);
                continue;
            }
            if (deadlineMs > 0 && Objects.nonNull(executor) && !executor.isShutdown()) {
                try {
                    executor.schedule(() -> {
                        if (!future.isDone()) {
                            // Note: RegisterUtils.doHeartBeat relies on OkHttp. Thread interruption does not immediately
                            // abort active socket writes on the wire, so cancel(true) is best-effort cancellation.
                            future.cancel(true);
                            if (Objects.nonNull(finalInFlight)) {
                                finalInFlight.set(false);
                            }
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
        LoginRetry retry = loginRetries.get(server);
        String token = null;
        try {
            token = getAccessToken(server);
            if (StringUtils.isBlank(token)) {
                return;
            }
            beatSender.doHeartBeat(GsonUtils.getInstance().toJson(instanceBeatInfoDTO), concat, Constants.HEARTBEAT, token);
            if (Objects.nonNull(retry)) {
                loginRetries.remove(server, retry);
            }
        } catch (HttpResponseException e) {
            if (e.getStatusCode() == HttpURLConnection.HTTP_UNAUTHORIZED && Objects.nonNull(token)) {
                invalidateRejectedToken(server, token);
            }
            LOG.error("HeartBeat admin url :{} returned HTTP {}, will retry.", server, e.getStatusCode());
        } catch (Exception e) {
            LOG.error("HeartBeat admin url :{} is fail, will retry.", server, e);
            // This is a periodic reporter, not a failback registration: retry on the next tick.
        }
    }

    private void invalidateRejectedToken(final String server, final String token) {
        accessToken.asMap().computeIfPresent(server, (key, cached) -> {
            // A timed-out request must not invalidate a token already refreshed by another worker.
            if (!Objects.equals(cached, token)) {
                return cached;
            }
            deferLogin(server);
            return null;
        });
    }

    private String getAccessToken(final String server) {
        String cached = accessToken.getIfPresent(server);
        if (StringUtils.isNotBlank(cached)) {
            return cached;
        }
        LoginRetry retry = loginRetries.get(server);
        if (Objects.nonNull(retry) && nanoTime.getAsLong() - retry.retryAtNanos < 0) {
            return null;
        }
        return accessToken.get(server);
    }

    private void deferLogin(final String server) {
        loginRetries.compute(server, (key, previous) -> {
            long delaySeconds = Objects.isNull(previous) ? CHECK_PERIOD : Math.min(previous.delaySeconds * 2, MAX_LOGIN_RETRY_DELAY_SECONDS);
            return new LoginRetry(delaySeconds, nanoTime.getAsLong() + TimeUnit.SECONDS.toNanos(delaySeconds));
        });
    }

    @EventListener(ContextClosedEvent.class)
    public void onShutdown() {
        closed = true;
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

    private static final class LoginRetry {

        private final long delaySeconds;

        private final long retryAtNanos;

        private LoginRetry(final long delaySeconds, final long retryAtNanos) {
            this.delaySeconds = delaySeconds;
            this.retryAtNanos = retryAtNanos;
        }
    }
}
