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
import org.apache.shenyu.register.common.protocol.RegistrationLeaseProtocol;
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

import java.io.IOException;
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
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.LongSupplier;

public class HeartbeatListener {

    private static final Logger LOG = LoggerFactory.getLogger(HeartbeatListener.class);

    private static final int INITIAL_DELAY = 0;

    private static final int CHECK_PERIOD = 5;

    private static final int MAX_BEAT_THREADS = 8;

    private static final long MAX_LOGIN_RETRY_DELAY_SECONDS = 60L;

    /**
     * Compact renewals carry the full instance observation only once per this many
     * consecutive renewals; the lease identity alone proves liveness in between.
     */
    private static final int LEASE_INFO_EVERY = 12;

    private static final String HEARTBEAT_TIMEOUT_MS = "heartbeatTimeoutMs";

    private static final String HEARTBEAT_DIRECT_EXECUTION = "heartbeatDirectExecution";

    /**
     * Client-side opt-in for the server-issued registration lease. When absent or
     * false, the reporter keeps the exact legacy v1 wire: no capability header and no
     * lease payload fields.
     */
    private static final String REGISTRATION_LEASE = "registrationLease";

    private static final long DEFAULT_DEADLINE_MS = 4000L;

    private final boolean directExecution;

    private ScheduledThreadPoolExecutor executor;

    private ExecutorService beatExecutor;

    private BeatSender beatSender;

    private final long deadlineMs;

    private final boolean leaseNegotiationEnabled;

    private final Map<String, AtomicBoolean> inFlight = new ConcurrentHashMap<>();

    private final ShenyuConfig shenyuConfig;

    private final LongSupplier nanoTime;

    // Heartbeat workers update retry state independently for each Admin.
    private final Map<String, LoginRetry> loginRetries = new ConcurrentHashMap<>();

    // Lease state independently for each Admin: a mixed cluster negotiates per server.
    private final Map<String, ServerLeaseState> leaseStates = new ConcurrentHashMap<>();

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
        this.leaseNegotiationEnabled = Boolean.parseBoolean(config.getProps().getProperty(REGISTRATION_LEASE));
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
            if (leaseNegotiationEnabled) {
                ServerLeaseState lease = leaseStates.computeIfAbsent(server, key -> new ServerLeaseState());
                if (!lease.isDue(nanoTime.getAsLong())) {
                    LOG.debug("Heartbeat to admin url :{} waits for its negotiated interval, skipping.", server);
                    continue;
                }
            }
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
                            if (leaseNegotiationEnabled) {
                                // a timed-out beat never produced a lease response, so the
                                // next attempt waits for the negotiated interval, not the tick
                                ServerLeaseState timedOut = leaseStates.get(server);
                                if (Objects.nonNull(timedOut)) {
                                    timedOut.deferAfterFailure(nanoTime.getAsLong());
                                }
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

    private void sendHeartbeatToServer(final String server, final InstanceBeatInfoDTO baseBeat) {
        String concat = server.concat(Constants.BEAT_URI_PATH);
        LoginRetry retry = loginRetries.get(server);
        if (!leaseNegotiationEnabled) {
            // exact legacy v1 wire: no capability header, no lease payload fields
            sendLegacyHeartbeat(server, concat, baseBeat, retry);
            return;
        }
        ServerLeaseState lease = leaseStates.computeIfAbsent(server, key -> new ServerLeaseState());
        String token = null;
        try {
            token = getAccessToken(server);
            if (StringUtils.isBlank(token)) {
                return;
            }
            String json = GsonUtils.getInstance().toJson(leasePayload(lease, baseBeat));
            String body = beatSender.doLeaseHeartBeat(json, concat, Constants.HEARTBEAT, token);
            handleBeatResponse(server, lease, body);
            if (Objects.nonNull(retry)) {
                loginRetries.remove(server, retry);
            }
        } catch (HttpResponseException e) {
            if (e.getStatusCode() == HttpURLConnection.HTTP_UNAUTHORIZED && Objects.nonNull(token)) {
                invalidateRejectedToken(server, token);
            }
            lease.deferAfterFailure(nanoTime.getAsLong());
            LOG.error("HeartBeat admin url :{} returned HTTP {}, will retry.", server, e.getStatusCode());
        } catch (Exception e) {
            lease.deferAfterFailure(nanoTime.getAsLong());
            LOG.error("HeartBeat admin url :{} is fail, will retry.", server, e);
            // This is a periodic reporter, not a failback registration: retry after a bounded, jittered delay.
        }
    }

    private void sendLegacyHeartbeat(final String server, final String url, final InstanceBeatInfoDTO instanceBeatInfoDTO,
                                     final LoginRetry retry) {
        String token = null;
        try {
            token = getAccessToken(server);
            if (StringUtils.isBlank(token)) {
                return;
            }
            beatSender.doHeartBeat(GsonUtils.getInstance().toJson(instanceBeatInfoDTO), url, Constants.HEARTBEAT, token);
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
        }
    }

    /**
     * Build the wire payload for one Admin: a compact lease renewal carries the
     * identity and the lease id, while the full instance observation is attached only
     * on (re-)registration and periodically, so a beat stays a liveness proof instead
     * of an accidental full re-registration.
     *
     * @param lease the lease state of the target Admin
     * @param baseBeat the observed instance state of this process
     * @return the payload for one beat
     */
    private InstanceBeatInfoDTO leasePayload(final ServerLeaseState lease, final InstanceBeatInfoDTO baseBeat) {
        InstanceBeatInfoDTO payload = new InstanceBeatInfoDTO();
        payload.setInstanceIp(baseBeat.getInstanceIp());
        payload.setInstancePort(baseBeat.getInstancePort());
        payload.setInstanceType(baseBeat.getInstanceType());
        payload.setNamespaceId(baseBeat.getNamespaceId());
        payload.setLeaseId(lease.getLeaseId());
        if (StringUtils.isBlank(lease.getLeaseId()) || lease.shouldRefreshObservation()) {
            payload.setInstanceInfo(baseBeat.getInstanceInfo());
        }
        return payload;
    }

    /**
     * Interpret one beat response body. A plain legacy marker keeps the v1 cadence, a
     * structured lease response advances or resets the lease state, and anything else
     * fails the send like the legacy sender did.
     *
     * @param server the target Admin url
     * @param lease the lease state of the target Admin
     * @param body the raw response body
     * @throws IOException when the body is neither the legacy marker nor a lease document
     */
    private void handleBeatResponse(final String server, final ServerLeaseState lease,
                                    final String body) throws IOException {
        final long now = nanoTime.getAsLong();
        if (Objects.equals(Constants.SUCCESS, body)) {
            lease.legacy(now);
            return;
        }
        if (StringUtils.isBlank(body) || !body.trim().startsWith("{")) {
            throw new IOException("Admin returned a non-success response for heartbeat at " + server);
        }
        RegistrationLeaseProtocol.LeaseResponse response =
                GsonUtils.getInstance().fromJson(body, RegistrationLeaseProtocol.LeaseResponse.class);
        if (Objects.isNull(response) || StringUtils.isBlank(response.code())) {
            throw new IOException("Admin returned an unreadable lease response at " + server);
        }
        switch (response.code()) {
            case RegistrationLeaseProtocol.OK -> lease.granted(response, now);
            case RegistrationLeaseProtocol.STALE_LEASE, RegistrationLeaseProtocol.EXPIRED,
                    RegistrationLeaseProtocol.LEASE_NOT_ENABLED, RegistrationLeaseProtocol.INVALID_REQUEST ->
                    lease.reRegister(now);
            default -> throw new IOException("Admin returned an unknown lease code at " + server);
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

        /**
         * Send heartbeat with the registration lease capability and return the raw
         * response body. The default delegates to the legacy sender and reports the
         * success marker, so senders that are not lease-aware keep their behavior.
         *
         * @param json json payload
         * @param url url
         * @param type type
         * @param token token
         * @return the raw response body
         * @throws Exception exception
         */
        default String doLeaseHeartBeat(String json, String url, String type, String token) throws Exception {
            doHeartBeat(json, url, type, token);
            return Constants.SUCCESS;
        }
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

        @Override
        public String doLeaseHeartBeat(final String json, final String url, final String type, final String token) throws Exception {
            return RegisterUtils.doLeaseHeartBeat(json, url, type, token);
        }
    }

    /**
     * Per-Admin lease state. A mixed cluster negotiates independently per server: a
     * server that answers with the legacy marker keeps the v1 cadence, a server that
     * grants a lease moves to the server-issued interval, and a rejected lease falls
     * back to immediate re-registration.
     */
    static final class ServerLeaseState {

        private volatile String leaseId;

        private volatile int intervalSeconds = CHECK_PERIOD;

        private volatile long nextDueNanos;

        private final AtomicInteger renewalCount = new AtomicInteger();

        String getLeaseId() {
            return leaseId;
        }

        boolean isDue(final long nowNanos) {
            return nowNanos >= nextDueNanos;
        }

        boolean shouldRefreshObservation() {
            return renewalCount.get() % LEASE_INFO_EVERY == 0;
        }

        void granted(final RegistrationLeaseProtocol.LeaseResponse response, final long nowNanos) {
            this.leaseId = response.leaseId();
            this.intervalSeconds = Objects.nonNull(response.intervalSeconds()) && response.intervalSeconds() > 0
                    ? response.intervalSeconds() : CHECK_PERIOD;
            renewalCount.incrementAndGet();
            this.nextDueNanos = nowNanos + TimeUnit.SECONDS.toNanos(intervalSeconds);
        }

        void reRegister(final long nowNanos) {
            this.leaseId = null;
            this.intervalSeconds = CHECK_PERIOD;
            renewalCount.set(0);
            // the next tick re-registers with a blank lease id and a full observation
            this.nextDueNanos = nowNanos;
        }

        void legacy(final long nowNanos) {
            this.leaseId = null;
            this.intervalSeconds = CHECK_PERIOD;
            this.nextDueNanos = nowNanos + TimeUnit.SECONDS.toNanos(CHECK_PERIOD);
        }

        void deferAfterFailure(final long nowNanos) {
            long base = TimeUnit.SECONDS.toNanos(intervalSeconds);
            long jitter = ThreadLocalRandom.current().nextLong(base / 10 + 1);
            this.nextDueNanos = nowNanos + base + jitter;
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
