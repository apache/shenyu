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

package org.apache.shenyu.plugin.agent.gateway.remote;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonObject;
import io.modelcontextprotocol.common.McpTransportContext;
import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.metrics.AgentMcpCatalogMetrics;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog.CatalogUnavailableException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import java.util.function.LongSupplier;

/** Managed, opt-in catalog fed exclusively by the trusted plugin data-sync channel. */
public final class ManagedRemoteMcpCatalog implements AgentMcpRemoteCatalog, AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(ManagedRemoteMcpCatalog.class);

    private static final ObjectMapper JSON = new ObjectMapper();

    private final RemoteCatalogLifecycle lifecycle;

    private final Set<URI> allowedEndpoints;

    private final Set<String> localNames;

    private final RemoteServiceCredentialResolver credentials;

    private final Scheduler scheduler;

    private final Scheduler timer;

    private final LongSupplier monotonicTime;

    private final AgentMcpCatalogMetrics metrics;

    private final Sinks.Many<ControlUpdate> updates = Sinks.many().unicast().onBackpressureBuffer(new ArrayBlockingQueue<>(16));

    private final AtomicLong revision = new AtomicLong();

    private final Set<Sinks.One<Long>> completions = new HashSet<>();

    private final Disposable worker;

    private volatile String lastFailure = "";

    private volatile boolean closed;

    private Disposable nextRefresh;

    private int consecutiveFailures;

    private long nextRefreshAt;

    private long refreshAttempts;

    private long refreshFailures;

    private long recoveries;

    private boolean remoteConfigured;

    private boolean published;

    private long lastSuccessNanos;

    private long freshnessNanos;

    /**
     * Managed Remote Mcp Catalog.
     * @param allowedEndpoints trusted allowedEndpoints value
     * @param localNames trusted localNames value
     * @param credentials trusted credentials value
     */
    public ManagedRemoteMcpCatalog(final Set<URI> allowedEndpoints, final Set<String> localNames, final RemoteServiceCredentialResolver credentials) {
        this(allowedEndpoints, localNames, credentials, false);
    }

    /**
     * Create a managed catalog with optional remote resource discovery.
     * @param allowedEndpoints trusted endpoint allowlist
     * @param localNames reserved local tool names
     * @param credentials trusted service credential resolver
     * @param resourcesEnabled opt in to remote resource discovery
     */
    public ManagedRemoteMcpCatalog(final Set<URI> allowedEndpoints, final Set<String> localNames, final RemoteServiceCredentialResolver credentials,
                                   final boolean resourcesEnabled) {
        this(allowedEndpoints, localNames, credentials, resourcesEnabled, Schedulers.newBoundedElastic(1, 16, "agent-mcp-config"),
                Schedulers.newSingle("agent-mcp-refresh"), System::nanoTime);
    }

    ManagedRemoteMcpCatalog(final Set<URI> allowedEndpoints, final Set<String> localNames, final RemoteServiceCredentialResolver credentials,
                           final Scheduler scheduler, final Scheduler timer) {
        this(allowedEndpoints, localNames, credentials, scheduler, timer, () -> timer.now(TimeUnit.NANOSECONDS));
    }

    ManagedRemoteMcpCatalog(final Set<URI> allowedEndpoints, final Set<String> localNames, final RemoteServiceCredentialResolver credentials,
                           final Scheduler scheduler, final Scheduler timer, final LongSupplier monotonicTime) {
        this(allowedEndpoints, localNames, credentials, false, scheduler, timer, monotonicTime);
    }

    ManagedRemoteMcpCatalog(final Set<URI> allowedEndpoints, final Set<String> localNames, final RemoteServiceCredentialResolver credentials,
                           final boolean resourcesEnabled, final Scheduler scheduler, final Scheduler timer) {
        this(allowedEndpoints, localNames, credentials, resourcesEnabled, scheduler, timer, () -> timer.now(TimeUnit.NANOSECONDS));
    }

    private ManagedRemoteMcpCatalog(final Set<URI> allowedEndpoints, final Set<String> localNames, final RemoteServiceCredentialResolver credentials,
                                    final boolean resourcesEnabled, final Scheduler scheduler, final Scheduler timer, final LongSupplier monotonicTime) {
        try {
            // Revision changes, withdrawal and final publication share the same monitor.
            lifecycle = new RemoteCatalogLifecycle(3, AgentGatewayAggregationConfig.DEFAULT_REFRESH_CONCURRENCY, resourcesEnabled, this);
            this.scheduler = Objects.requireNonNull(scheduler, "scheduler");
            this.timer = Objects.requireNonNull(timer, "timer");
            this.monotonicTime = Objects.requireNonNull(monotonicTime, "monotonicTime");
            this.allowedEndpoints = Set.copyOf(allowedEndpoints);
            this.localNames = Set.copyOf(localNames);
            this.credentials = Objects.requireNonNull(credentials, "credentials");
            this.allowedEndpoints.forEach(RemoteTransportPolicy::endpoint);
            // The single owned subscription belongs to the control-plane lifecycle, never a request.
            lifecycle.refresh(List.of()).block(Duration.ofSeconds(6));
            worker = updates.asFlux().publishOn(scheduler, 1).concatMap(update -> applyUpdate(update).subscribeOn(scheduler), 1).subscribe();
            metrics = new AgentMcpCatalogMetrics(this::metricsSnapshot);
            if (!metrics.register()) {
                LOG.warn("Agent MCP catalog metrics source budget exhausted");
            }
        } catch (RuntimeException error) {
            if (Objects.nonNull(scheduler)) {
                scheduler.dispose();
            }
            if (Objects.nonNull(timer)) {
                timer.dispose();
            }
            throw error;
        }
    }

    private Mono<Long> applyUpdate(final ControlUpdate update) {
        return Mono.defer(() -> {
            if (!isCurrent(update)) {
                complete(update, null, null);
                return lifecycle.withdrawAsync().then(Mono.empty());
            }
            if (!update.rejection().isEmpty()) {
                recordFailure(update.sequence(), update.rejection());
                return lifecycle.withdrawAsync().then(Mono.defer(() -> {
                    complete(update, null, new IllegalArgumentException(update.rejection()));
                    return Mono.empty();
                }));
            }
            AgentGatewayAggregationConfig config;
            try {
                config = AgentGatewayAggregationConfig.parsePluginConfig(update.source());
            } catch (IllegalArgumentException error) {
                recordFailure(update.sequence(), error.getClass().getSimpleName());
                complete(update, null, error);
                return lifecycle.withdrawAsync().then(Mono.empty());
            }
            if (!beginRefresh(update, config)) {
                complete(update, null, null);
                return Mono.empty();
            }
            return lifecycle.withdrawAsync().then(refreshCatalog(config, () -> isCurrent(update)))
                    .doOnNext(version -> refreshSucceeded(update, config, version))
                    .onErrorResume(error -> lifecycle.withdrawAsync().then(Mono.defer(() -> {
                        refreshFailed(update, config, error);
                        return Mono.empty();
                    })))
                    .switchIfEmpty(Mono.defer(() -> {
                        complete(update, null, null);
                        return Mono.empty();
                    }));
        });
    }

    private boolean isCurrent(final ControlUpdate update) {
        return !closed && update.sequence() == revision.get();
    }

    private synchronized boolean beginRefresh(final ControlUpdate update, final AgentGatewayAggregationConfig config) {
        if (!isCurrent(update)) {
            return false;
        }
        remoteConfigured = !config.servers().isEmpty();
        if (remoteConfigured) {
            refreshAttempts++;
        }
        return true;
    }

    private synchronized void refreshSucceeded(final ControlUpdate update, final AgentGatewayAggregationConfig config, final long version) {
        if (isCurrent(update)) {
            if (remoteConfigured) {
                if (consecutiveFailures > 0) {
                    recoveries++;
                    LOG.info("Agent MCP remote catalog recovered");
                }
            }
            consecutiveFailures = 0;
            lastFailure = "";
            scheduleNext(update, config, config.refreshPolicy().nextDelay(0));
            complete(update, version, null);
        } else {
            complete(update, null, null);
        }
    }

    private synchronized void recordPublication(final AgentGatewayAggregationConfig config) {
        published = !config.servers().isEmpty();
        lastSuccessNanos = monotonicTime.getAsLong();
        freshnessNanos = config.refreshPolicy().refreshInterval().toNanos();
    }

    // Called under the lifecycle's shared monitor, atomically with lease acquisition.
    private boolean isFresh() {
        return !remoteConfigured || published && Math.max(0L, monotonicTime.getAsLong() - lastSuccessNanos) < freshnessNanos;
    }

    private synchronized void refreshFailed(final ControlUpdate update, final AgentGatewayAggregationConfig config, final Throwable error) {
        if (isCurrent(update)) {
            if (remoteConfigured) {
                refreshFailures++;
            }
            lastFailure = error.getClass().getSimpleName();
            consecutiveFailures = Math.min(consecutiveFailures + 1, config.refreshPolicy().maxRetries() + 1);
            LOG.warn("Agent MCP catalog refresh failed ({})", lastFailure);
            Duration delay = error instanceof SecurityException || error instanceof IllegalArgumentException
                    ? config.refreshPolicy().refreshInterval() : config.refreshPolicy().nextDelay(consecutiveFailures);
            scheduleNext(update, config, delay);
        }
        complete(update, null, error);
    }

    private void scheduleNext(final ControlUpdate update, final AgentGatewayAggregationConfig config, final Duration delay) {
        cancelNextRefresh();
        if (!config.servers().isEmpty() && lifecycle.diagnosticsState().cleanupFailures().isEmpty()) {
            nextRefreshAt = timer.now(TimeUnit.MILLISECONDS) + delay.toMillis();
            ControlUpdate scheduled = new ControlUpdate(update.sequence(), update.source());
            nextRefresh = timer.schedule(() -> enqueueScheduled(scheduled), delay.toMillis(), TimeUnit.MILLISECONDS);
        }
    }

    private synchronized void enqueueScheduled(final ControlUpdate update) {
        if (isCurrent(update)) {
            nextRefresh = null;
            nextRefreshAt = 0;
            enqueue(update);
        }
    }

    private void cancelNextRefresh() {
        if (Objects.nonNull(nextRefresh)) {
            nextRefresh.dispose();
            nextRefresh = null;
        }
        nextRefreshAt = 0;
    }

    /**
     * Enqueue an immutable data-sync event without blocking its callback thread.
     * @param plugin authoritative plugin data
     */
    public synchronized void accept(final PluginData plugin) {
        if (closed) {
            return;
        }
        final long sequence = revision.incrementAndGet();
        cancelNextRefresh();
        consecutiveFailures = 0;
        remoteConfigured = false;
        published = false;
        // Diagnostics belong to the newest event, even if an older update is fenced before reserve().
        lastFailure = "";
        boolean enabled = Boolean.TRUE.equals(plugin.getEnabled());
        // Withdraw before resolving replacement credentials; late old results cannot publish.
        lifecycle.withdraw();
        String source = enabled && Objects.nonNull(plugin.getConfig()) ? plugin.getConfig() : "{}";
        if (source.getBytes(StandardCharsets.UTF_8).length > 16384) {
            lifecycle.withdraw();
            lastFailure = "ConfigurationByteLimit";
            enqueue(new ControlUpdate(sequence, null, null, lastFailure));
            return;
        }
        enqueue(new ControlUpdate(sequence, source));
    }

    private void enqueue(final ControlUpdate update) {
        Sinks.EmitResult result = updates.tryEmitNext(update);
        if (result.isFailure() && !closed) {
            lifecycle.withdraw();
            lastFailure = "ConfigurationQueueRejected";
            LOG.warn("Agent MCP configuration queue rejected an update; remote access withdrawn");
            complete(update, null, new CatalogUnavailableException());
        }
    }

    /**
     * Replace configuration through the same validated operation used by data sync.
     * The owned worker resolves credentials; request cancellation does not replay tool calls.
     * @param source complete trusted plugin config
     * @return published directory generation
     */
    public Mono<Long> reload(final String source) {
        return Mono.defer(() -> {
            Sinks.One<Long> completion = Sinks.one();
            synchronized (this) {
                if (closed) {
                    return Mono.error(new CatalogUnavailableException());
                }
                final long sequence = revision.incrementAndGet();
                cancelNextRefresh();
                consecutiveFailures = 0;
                remoteConfigured = false;
                published = false;
                lastFailure = "";
                lifecycle.withdraw();
                completions.add(completion);
                if (Objects.nonNull(source) && source.getBytes(StandardCharsets.UTF_8).length > 16384) {
                    lastFailure = "ConfigurationByteLimit";
                    enqueue(new ControlUpdate(sequence, null, completion, lastFailure));
                } else {
                    enqueue(new ControlUpdate(sequence, source, completion));
                }
            }
            return completion.asMono();
        });
    }

    private synchronized void complete(final ControlUpdate update, final Long version, final Throwable error) {
        if (Objects.nonNull(update.completion())) {
            completions.remove(update.completion());
            if (Objects.nonNull(error)) {
                update.completion().tryEmitError(error);
            } else if (Objects.nonNull(version)) {
                update.completion().tryEmitValue(version);
            } else {
                update.completion().tryEmitEmpty();
            }
        }
    }

    private Mono<Long> refreshCatalog(final AgentGatewayAggregationConfig config, final BooleanSupplier stillCurrent) {
        return Mono.defer(() -> {
            List<RemoteCatalogLifecycle.Config> targets = new ArrayList<>();
            for (AgentGatewayAggregationConfig.Server server : config.servers()) {
                RemoteServerBinding.Config reference = new RemoteServerBinding.Config(server.name(), server.endpoint(), server.credentialRef(), server.credentialVersion());
                targets.add(RemoteServerBinding.resolve(reference, allowedEndpoints, credentials::resolve).lifecycleConfig());
            }
            return lifecycle.refresh(targets,
                    List.of(),
                    localNames,
                    stillCurrent,
                    config.refreshConcurrency(),
                    ignored -> recordPublication(config));
        });
    }

    @Override
    public Mono<ObjectNode> withSnapshot(final Function<Snapshot, Mono<ObjectNode>> operation) {
        return lifecycle.withSnapshot(directory -> operation.apply(new CatalogSnapshot(directory)), this::isFresh);
    }

    /**
     * Secret-free lifecycle diagnostics for management and verification.
     * @return immutable diagnostic values
     */
    public Map<String, Object> diagnostics() {
        Diagnostics state = diagnosticsState();
        Map<String, Object> result = new LinkedHashMap<>(state.lifecycle().toMap());
        result.put("lastFailure", state.lastFailure());
        result.put("refreshAttempts", state.refresh().attempts());
        result.put("refreshFailures", state.refresh().failures());
        result.put("recoveries", state.refresh().recoveries());
        result.put("consecutiveFailures", state.refresh().consecutiveFailures());
        result.put("lastSuccessAgeMillis", state.refresh().lastSuccessAgeMillis());
        result.put("remoteConfigured", state.refresh().configured());
        result.put("available", state.refresh().available());
        result.put("failureActive", state.refresh().failureActive());
        return Map.copyOf(result);
    }

    /**
     * Capture typed lifecycle and current control-update failure diagnostics.
     * @return immutable control-plane diagnostic snapshot
     */
    public synchronized Diagnostics diagnosticsState() {
        return new Diagnostics(lifecycle.diagnosticsState(), revision.get(), lastFailure, metricsSnapshot());
    }

    private synchronized AgentMcpCatalogMetrics.Snapshot metricsSnapshot() {
        long age = published ? TimeUnit.NANOSECONDS.toMillis(Math.max(0L, monotonicTime.getAsLong() - lastSuccessNanos)) : -1L;
        boolean available = !closed && remoteConfigured && isFresh() && lifecycle.diagnosticsState().currentVersion() > 0L;
        return new AgentMcpCatalogMetrics.Snapshot(refreshAttempts, refreshFailures, recoveries, consecutiveFailures, age,
                remoteConfigured, available, !lastFailure.isEmpty());
    }

    synchronized RefreshTiming refreshTiming() {
        return new RefreshTiming(consecutiveFailures, nextRefreshAt);
    }

    private synchronized void recordFailure(final long sequence, final String category) {
        if (sequence == revision.get()) {
            lastFailure = category;
        }
    }

    /**
     * Stop owned configuration work and drain the catalog before process exit.
     * @return cleanup completion, or explicit cleanup failure
     */
    public Mono<Void> closeAsync() {
        return Mono.defer(() -> {
            synchronized (this) {
                closed = true;
                revision.incrementAndGet();
                cancelNextRefresh();
                lifecycle.withdraw();
                remoteConfigured = false;
                published = false;
                metrics.unregister();
                List<Sinks.One<Long>> waiting = List.copyOf(completions);
                completions.clear();
                waiting.forEach(completion -> completion.tryEmitError(new CatalogUnavailableException()));
            }
            worker.dispose();
            timer.dispose();
            scheduler.dispose();
            return lifecycle.closeGracefully();
        });
    }

    @Override
    public void close() {
        closeAsync().block(Duration.ofSeconds(8));
    }

    /** Failure is scoped to revision; empty means no failure recorded, not a ready catalog. */
    public record Diagnostics(RemoteCatalogLifecycle.Diagnostics lifecycle, long revision, String lastFailure, AgentMcpCatalogMetrics.Snapshot refresh) {
    }

    private record ControlUpdate(long sequence, String source, Sinks.One<Long> completion, String rejection) {

        private ControlUpdate(final long sequence, final String source) {
            this(sequence, source, null, "");
        }

        private ControlUpdate(final long sequence, final String source, final Sinks.One<Long> completion) {
            this(sequence, source, completion, "");
        }
    }

    record RefreshTiming(int consecutiveFailures, long nextRefreshAt) {
    }

    private static final class CatalogSnapshot implements Snapshot {

        private final RemoteToolDirectory directory;

        private CatalogSnapshot(final RemoteToolDirectory directory) {
            this.directory = directory;
        }

        @Override
        public Map<String, ObjectNode> definitions() {
            Map<String, ObjectNode> result = new LinkedHashMap<>();
            directory.definitions(directory.names(), directory.names()).forEach((name, tool) -> result.put(name, JSON.valueToTree(tool)));
            return Map.copyOf(result);
        }

        @Override
        public Map<String, ObjectNode> resourceDefinitions() {
            return directory.resources().definitions();
        }

        @Override
        public Mono<ObjectNode> readResource(final String uri, final AgentMcpExecutionContext context) {
            return directory.resources().read(uri, context.getAllowedResources())
                    .contextWrite(value -> value.put(McpTransportContext.KEY, McpTransportContext.create(Map.of("deadline", context.getDeadline()))));
        }

        @Override
        public Mono<ObjectNode> invoke(final String name, final JsonObject arguments, final AgentMcpExecutionContext context) {
            Map<String, Object> values;
            try {
                values = JSON.readValue(arguments.toString(), new TypeReference<Map<String, Object>>() { });
            } catch (Exception error) {
                return Mono.error(new IllegalArgumentException("Invalid private arguments"));
            }
            return directory
                .callRaw(name, context.getRuleTools(), context.getAllowedTools(), values)
                .contextWrite(value -> value.put(McpTransportContext.KEY, McpTransportContext.create(Map.of("subject", context.getSubject(), "deadline", context.getDeadline()))));
        }
    }
}
