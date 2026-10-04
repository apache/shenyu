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

package org.apache.shenyu.plugin.sync.data.websocket.client;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.constant.RunningModeConstants;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.dto.WebsocketSyncFrame;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.RunningModeEnum;
import org.apache.shenyu.common.timer.AbstractRoundTask;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.TimerTask;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.plugin.sync.data.websocket.handler.WebsocketDataHandler;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.apache.shenyu.common.concurrent.MemorySafeTaskQueue;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The type shenyu websocket client.
 */
public final class ShenyuWebsocketClient extends WebSocketClient {
    
    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuWebsocketClient.class);

    private static final int RECONNECT_EXECUTOR_CORE_POOL_SIZE = 1;

    private static final int RECONNECT_EXECUTOR_MAX_POOL_SIZE = 8;

    private static final long RECONNECT_EXECUTOR_KEEP_ALIVE_MS = TimeUnit.SECONDS.toMillis(60);

    private static final ExecutorService RECONNECT_EXECUTOR = new ShenyuThreadPoolExecutor(
            RECONNECT_EXECUTOR_CORE_POOL_SIZE,
            RECONNECT_EXECUTOR_MAX_POOL_SIZE,
            RECONNECT_EXECUTOR_KEEP_ALIVE_MS,
            TimeUnit.MILLISECONDS,
            new MemorySafeTaskQueue<>(Constants.THE_256_MB),
            ShenyuThreadFactory.create("websocket-reconnect", true),
            new ThreadPoolExecutor.AbortPolicy());

    private static final long MIN_RECONNECT_BACKOFF_MS = TimeUnit.SECONDS.toMillis(1);

    private static final long MAX_RECONNECT_BACKOFF_MS = TimeUnit.SECONDS.toMillis(60);

    private static final int MAX_CONSECUTIVE_SYNC_FAILURES = 3;

    private volatile boolean alreadySync = Boolean.FALSE;

    private InitialSyncState initialSyncState;

    private volatile long nextSyncRetryAt;


    private final WebsocketDataHandler websocketDataHandler;

    private final Timer timer;

    private TimerTask timerTask;

    private String runningMode;

    private String masterUrl;

    private volatile boolean isConnectedToMaster;

    private final String namespaceId;

    private final AtomicBoolean manuallyClosed = new AtomicBoolean(false);

    private final AtomicBoolean reconnecting = new AtomicBoolean(false);

    private final AtomicInteger consecutiveSyncFailures = new AtomicInteger(0);

    private volatile long lastReconnectAttemptTime;

    private final AtomicInteger reconnectBackoff = new AtomicInteger(0);

    private volatile Thread reconnectThread;

    /**
     * Instantiates a new shenyu websocket client.
     *
     * @param serverUri the server uri
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers the meta data subscribers
     * @param authDataSubscribers the auth data subscribers
     * @param proxySelectorDataSubscribers proxySelectorDataSubscribers,
     * @param discoveryUpstreamDataSubscribers discoveryUpstreamDataSubscribers,
     */
    public ShenyuWebsocketClient(final URI serverUri,
                                 final PluginDataSubscriber pluginDataSubscriber,
                                 final List<MetaDataSubscriber> metaDataSubscribers,
                                 final List<AuthDataSubscriber> authDataSubscribers,
                                 final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                 final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers,
                                 final List<AiProxyApiKeyDataSubscriber> aiProxyApiKeyDataSubscribers,
                                 final String namespaceId,
                                 final Integer port
    ) {
        super(serverUri);
        this.namespaceId = namespaceId;
        this.addHeader(Constants.SHENYU_NAMESPACE_ID, namespaceId);
        this.addHeader(Constants.CLIENT_PORT_NAME, String.valueOf(port));
        this.websocketDataHandler = new WebsocketDataHandler(
                pluginDataSubscriber,
                metaDataSubscribers,
                authDataSubscribers,
                proxySelectorDataSubscribers,
                discoveryUpstreamDataSubscribers,
                aiProxyApiKeyDataSubscribers
        );
        this.timer = WheelTimerFactory.getSharedTimer();
        this.connection();
    }
    
    /**
     * Instantiates a new shenyu websocket client.
     *
     * @param serverUri the server uri
     * @param headers the headers
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers the meta data subscribers
     * @param authDataSubscribers the auth data subscribers
     * @param proxySelectorDataSubscribers proxySelectorDataSubscribers,
     * @param discoveryUpstreamDataSubscribers discoveryUpstreamDataSubscribers,
     */
    public ShenyuWebsocketClient(final URI serverUri,
                                 final Map<String, String> headers,
                                 final PluginDataSubscriber pluginDataSubscriber,
                                 final List<MetaDataSubscriber> metaDataSubscribers,
                                 final List<AuthDataSubscriber> authDataSubscribers,
                                 final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                 final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers,
                                 final List<AiProxyApiKeyDataSubscriber> aiProxyApiKeyDataSubscribers,
                                 final String namespaceId,
                                 final Integer port) {
        this(serverUri, headers, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers, aiProxyApiKeyDataSubscribers,
                namespaceId, port, null);
    }

    /**
     * Create a client with an optional startup readiness latch.
     * @param serverUri server URI
     * @param headers headers
     * @param pluginDataSubscriber plugin subscriber
     * @param metaDataSubscribers metadata subscribers
     * @param authDataSubscribers authorization subscribers
     * @param proxySelectorDataSubscribers proxy selector subscribers
     * @param discoveryUpstreamDataSubscribers discovery subscribers
     * @param aiProxyApiKeyDataSubscribers API key subscribers
     * @param namespaceId namespace
     * @param port gateway port
     * @param initialSyncReady startup latch, null for the legacy protocol
     */
    public ShenyuWebsocketClient(final URI serverUri,
                                 final Map<String, String> headers,
                                 final PluginDataSubscriber pluginDataSubscriber,
                                 final List<MetaDataSubscriber> metaDataSubscribers,
                                 final List<AuthDataSubscriber> authDataSubscribers,
                                 final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                 final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers,
                                 final List<AiProxyApiKeyDataSubscriber> aiProxyApiKeyDataSubscribers,
                                 final String namespaceId,
                                 final Integer port,
                                 final AtomicBoolean initialSyncReady) {
        super(serverUri, headers);
        if (Objects.nonNull(initialSyncReady)) {
            this.initialSyncState = new InitialSyncState(initialSyncReady);
        }
        this.namespaceId = namespaceId;
        LOG.info("shenyu bootstrap websocket namespaceId: {}", namespaceId);
        this.addHeader(Constants.SHENYU_NAMESPACE_ID, namespaceId);
        this.addHeader(Constants.CLIENT_PORT_NAME, String.valueOf(port));
        this.websocketDataHandler = new WebsocketDataHandler(
                pluginDataSubscriber,
                metaDataSubscribers,
                authDataSubscribers,
                proxySelectorDataSubscribers,
                discoveryUpstreamDataSubscribers,
                aiProxyApiKeyDataSubscribers
        );
        this.timer = WheelTimerFactory.getSharedTimer();
        this.connection();
    }
    
    private void connection() {
        if (Objects.nonNull(initialSyncState)) {
            // Management endpoints must start even when Admin is unavailable.
            this.connect();
        } else {
            this.connectBlocking();
        }
        this.timer.add(timerTask = new AbstractRoundTask(null, TimeUnit.SECONDS.toMillis(10)) {
            @Override
            public void doRun(final String key, final TimerTask timerTask) {
                healthCheck();
            }
        });
    }
    
    @Override
    public boolean connectBlocking() {
        boolean success = false;
        try {
            success = super.connectBlocking();
        } catch (Exception exception) {
            LOG.error("websocket connection server[{}] is error.....[{}]", this.getURI().toString(), exception.getMessage());
        }
        if (success) {
            LOG.info("websocket connection server[{}] is successful.....", this.getURI().toString());
        } else {
            LOG.warn("websocket connection server[{}] is error.....", this.getURI().toString());
        }
        return success;
    }
    
    @Override
    public void onOpen(final ServerHandshake serverHandshake) {
        LOG.info("websocket connection server[{}] is opened, sending sync msg", this.getURI().toString());
        send(DataEventTypeEnum.RUNNING_MODE.name());
        if (!alreadySync) {
            if (Objects.nonNull(initialSyncState)) {
                send(WebsocketSyncFrame.REQUEST_PREFIX + initialSyncState.begin());
            } else {
                send(DataEventTypeEnum.MYSELF.name());
            }
            alreadySync = true;
        }
    }
    
    @Override
    public void onMessage(final String result) {
        final Map<String, Object> jsonToMap;
        try {
            jsonToMap = JsonUtils.jsonToMap(result);
            if (Objects.isNull(jsonToMap)) {
                return;
            }
        } catch (RuntimeException ex) {
            LOG.warn("Ignoring unparseable websocket frame from server[{}]", this.getURI());
            return;
        }
        try {
            Object eventType = jsonToMap.get(RunningModeConstants.EVENT_TYPE);
            if (Objects.equals(WebsocketSyncFrame.EVENT_TYPE, eventType) && Objects.nonNull(initialSyncState)) {
                initialSyncState.accept(GsonUtils.getInstance().fromJson(result, WebsocketSyncFrame.class), this::handleResult);
            } else if (Objects.equals(DataEventTypeEnum.RUNNING_MODE.name(), eventType)) {
                this.runningMode = String.valueOf(jsonToMap.get(RunningModeConstants.RUNNING_MODE));
                if (!Objects.equals(RunningModeEnum.STANDALONE.name(), runningMode)) {
                    this.masterUrl = String.valueOf(jsonToMap.get(RunningModeConstants.MASTER_URL));
                    this.isConnectedToMaster = Boolean.TRUE.equals(jsonToMap.get(RunningModeConstants.IS_MASTER));
                }
            } else if (Objects.nonNull(initialSyncState)) {
                initialSyncState.applyIncremental(() -> handleResult(result));
            } else {
                handleResult(result);
            }
        } catch (RuntimeException ex) {
            if (Objects.nonNull(initialSyncState)) {
                initialSyncState.invalidate();
            }
            LOG.warn("Failed to handle websocket frame from server[{}]", this.getURI(), ex);
        }
    }

    @Override
    public void onClose(final int i, final String s, final boolean b) {
        this.close();
    }
    
    @Override
    public void onError(final Exception e) {
        LOG.error("websocket server[{}] is error.....", getURI(), e);
    }
    
    @Override
    public void close() {
        if (Objects.nonNull(initialSyncState)) {
            initialSyncState.invalidate();
        }
        alreadySync = false;
        if (this.isOpen()) {
            super.close();
        }
    }
    
    /**
     * Now close.
     * now close. will cancel the task execution.
     */
    public void nowClose() {
        this.manuallyClosed.set(true);
        if (Objects.nonNull(timerTask)) {
            timerTask.cancel();
        }
        Thread currentReconnectThread = this.reconnectThread;
        if (Objects.nonNull(currentReconnectThread)) {
            currentReconnectThread.interrupt();
        }
        this.close();
    }
    
    private void healthCheck() {
        try {
            if (this.manuallyClosed.get()) {
                return;
            }
            if (nextSyncRetryAt != 0) {
                if (System.nanoTime() - nextSyncRetryAt < 0) {
                    return;
                }
                nextSyncRetryAt = 0;
                close();
                return;
            }
            if (Objects.nonNull(initialSyncState) && this.isOpen() && initialSyncState.needsReconnect()) {
                close();
                return;
            }
            if (!this.isOpen()) {
                if (this.reconnecting.compareAndSet(false, true)) {
                    RECONNECT_EXECUTOR.submit(this::doReconnect);
                }
            } else {
                this.reconnectBackoff.set(0);
                this.sendPing();
                send(getInstanceInfo());
                LOG.debug("websocket send to [{}] ping message successful", this.getURI());
            }
        } catch (Exception e) {
            LOG.error("websocket connect is error :{}", e.getMessage());
        }
    }

    private void doReconnect() {
        this.reconnectThread = Thread.currentThread();
        try {
            if (this.manuallyClosed.get()) {
                return;
            }
            long backoff = calculateBackoff();
            long since = System.currentTimeMillis() - lastReconnectAttemptTime;
            long waitMs = backoff - since;
            if (waitMs > 0) {
                Thread.sleep(waitMs);
            }
            try {
                this.reconnectBlocking();
            } finally {
                lastReconnectAttemptTime = System.currentTimeMillis();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            reconnectBackoff.set(Math.min(reconnectBackoff.get() + 1, 10));
            LOG.error("websocket reconnect server[{}] error", this.getURI(), e);
        } finally {
            this.reconnectThread = null;
            this.reconnecting.set(false);
            if (this.manuallyClosed.get()) {
                this.close();
            }
        }
    }

    private long calculateBackoff() {
        int failures = reconnectBackoff.get();
        if (failures <= 0) {
            return 0;
        }
        long base = Math.min(
                MIN_RECONNECT_BACKOFF_MS * (1L << Math.min(failures - 1, 10)),
                MAX_RECONNECT_BACKOFF_MS);
        return base + (long) (base * 0.5 * ThreadLocalRandom.current().nextDouble());
    }

    private String getInstanceInfo() {
        // Combine instance and host information
        Map<String, Object> combinedInfo = Map.of(
                InstanceTypeConstants.BOOTSTRAP_INSTANCE_INFO, SystemInfoUtils.getSystemInfo()
        );

        return GsonUtils.getInstance().toJson(combinedInfo);
    }

    /**
     * handle admin message.
     *
     * @param result result
     */
    private void handleResult(final String result) {
        LOG.info("server [{}] handleResult({})", this.getURI().toString(), result);
        WebsocketData<?> websocketData;
        try {
            websocketData = GsonUtils.getInstance().fromJson(result, WebsocketData.class);
        } catch (RuntimeException ex) {
            // a frame that cannot be interpreted as a config message is a protocol-shape
            // mismatch, not a recoverable config change: ignore it like the previous
            // behavior instead of dropping the connection
            LOG.warn("Failed to parse websocket message from server[{}], the message will be ignored", this.getURI(), ex);
            return;
        }
        ConfigGroupEnum groupEnum;
        String eventType;
        String json;
        try {
            groupEnum = ConfigGroupEnum.acquireByName(websocketData.getGroupType());
            eventType = websocketData.getEventType();
            DataEventTypeEnum.acquireByName(eventType);
            json = GsonUtils.getInstance().toJson(websocketData.getData());
        } catch (RuntimeException ex) {
            LOG.warn("Failed to resolve websocket message group from server[{}], the message will be ignored", this.getURI(), ex);
            return;
        }
        try {
            if (websocketData.isFullSnapshot()) {
                if (!DataEventTypeEnum.REFRESH.name().equals(eventType) && !DataEventTypeEnum.MYSELF.name().equals(eventType)) {
                    throw new IllegalArgumentException("Snapshot requires a refresh event");
                }
                websocketDataHandler.snapshot(groupEnum, json, websocketData.getNamespaceId(), namespaceId);
            } else {
                websocketDataHandler.executor(groupEnum, json, eventType);
            }
            consecutiveSyncFailures.set(0);
        } catch (RuntimeException ex) {
            handleSyncFailure(ex, groupEnum.name(), eventType);
            if (org.apache.shenyu.common.utils.InitialSyncApplication.isActive()) {
                throw ex;
            }
        }
    }

    /**
     * Recover failed configuration application without abandoning the timer.
     * @param ex application failure
     * @param groupType configuration group
     * @param eventType event type
     */
    private void handleSyncFailure(final RuntimeException ex, final String groupType, final String eventType) {
        int failures = consecutiveSyncFailures.updateAndGet(value -> Math.min(value + 1, MAX_CONSECUTIVE_SYNC_FAILURES));
        if (failures >= MAX_CONSECUTIVE_SYNC_FAILURES) {
            if (nextSyncRetryAt == 0) {
                nextSyncRetryAt = System.nanoTime() + MAX_RECONNECT_BACKOFF_MS * 1_000_000L;
            }
            LOG.warn("websocket sync failed, group={}, eventType={}; full resync scheduled after backoff", groupType, eventType, ex);
            return;
        }
        LOG.warn("websocket sync failed, group={}, eventType={}, consecutiveFailures={}; reconnecting for full resync",
                groupType, eventType, failures, ex);
        this.close();
    }

    /**
     * Gets the master url.
     *
     * @return the master url
     */
    public String getMasterUrl() {
        return masterUrl;
    }
    
    /**
     * Gets the running mode.
     *
     * @return the running mode
     */
    public String getRunningMode() {
        return runningMode;
    }
    
    /**
     * whether connect to master.
     *
     * @return whether connect to master
     */
    public boolean isConnectedToMaster() {
        return isConnectedToMaster;
    }
    
}
