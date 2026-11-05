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
import org.apache.shenyu.common.dto.AgentGatewayAdmissionConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.apache.shenyu.common.metrics.AgentCatalogMetrics;
import org.apache.shenyu.common.tracing.AgentTraceContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpResourceAdmission;
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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.BooleanSupplier;
import java.util.function.Function;

/** Managed, opt-in catalog fed exclusively by the trusted plugin data-sync channel. */
public final class ManagedRemoteMcpCatalog implements AgentMcpRemoteCatalog, AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(ManagedRemoteMcpCatalog.class);

    private static final ObjectMapper JSON = new ObjectMapper();

    private final RemoteCatalogLifecycle lifecycle;

    private final RemoteTransportRouteSet routes;

    private final Set<String> localNames;

    private final RemoteServiceCredentialResolver credentials;

    private final javax.net.ssl.SSLContext verificationTlsContext;

    private final AgentMcpResourceAdmission.ResourceAdmissionController admission;

    private final Scheduler scheduler = Schedulers.newBoundedElastic(1, 16, "agent-mcp-config");

    private final Sinks.Many<ControlUpdate> updates = Sinks.many().unicast().onBackpressureBuffer(new ArrayBlockingQueue<>(16));

    private final AtomicLong revision = new AtomicLong();

    private final Disposable worker;

    private final AgentCatalogMetrics catalogMetrics;

    private volatile String lastFailure = "";

    private volatile boolean closed;

    /**
     * Managed catalog compatibility constructor for fixed-IP deployments.
     * @param allowedEndpoints trusted fixed-IP endpoints
     * @param localNames local tool names
     * @param credentials fixed service credential resolver
     */
    public ManagedRemoteMcpCatalog(final Set<URI> allowedEndpoints, final Set<String> localNames, final RemoteServiceCredentialResolver credentials) {
        this(allowedEndpoints, localNames, credentials, AgentMcpResourceAdmission.create(AgentGatewayAdmissionConfig.parse(null)), false);
    }

    /**
     * Managed catalog with a shared admission controller.
     * @param allowedEndpoints trusted fixed-IP endpoints
     * @param localNames local tool names
     * @param credentials fixed service credential resolver
     * @param admission shared controller
     */
    public ManagedRemoteMcpCatalog(final Set<URI> allowedEndpoints, final Set<String> localNames,
                                   final RemoteServiceCredentialResolver credentials,
                                   final AgentMcpResourceAdmission.ResourceAdmissionController admission) {
        this(allowedEndpoints, localNames, credentials, admission, false);
    }

    /**
     * Managed catalog with shared admission and resource lifecycle configuration.
     * @param allowedEndpoints trusted fixed-IP endpoints
     * @param localNames local tool names
     * @param credentials fixed service credential resolver
     * @param admission shared controller
     * @param resourcesEnabled whether remote resources are enabled
     */
    public ManagedRemoteMcpCatalog(final Set<URI> allowedEndpoints, final Set<String> localNames,
                                   final RemoteServiceCredentialResolver credentials,
                                   final AgentMcpResourceAdmission.ResourceAdmissionController admission, final boolean resourcesEnabled) {
        this(fixedRoutes(allowedEndpoints), localNames, credentials, admission, resourcesEnabled, null);
    }

    private ManagedRemoteMcpCatalog(final Set<String> allowedRoutes, final Set<String> localNames,
                                    final RemoteServiceCredentialResolver credentials,
                                    final AgentMcpResourceAdmission.ResourceAdmissionController admission,
                                    final boolean resourcesEnabled, final javax.net.ssl.SSLContext verificationTlsContext) {
        lifecycle = new RemoteCatalogLifecycle(3, resourcesEnabled);
        this.routes = RemoteTransportRouteSet.parse(allowedRoutes);
        this.localNames = Set.copyOf(localNames);
        this.credentials = Objects.requireNonNull(credentials, "credentials");
        this.admission = Objects.requireNonNull(admission, "admission");
        this.verificationTlsContext = verificationTlsContext;
        // The single owned subscription belongs to the control-plane lifecycle, never a request.
        lifecycle.refresh(List.of()).block(Duration.ofSeconds(6));
        worker = updates.asFlux().publishOn(scheduler, 1).concatMap(this::applyUpdate, 1).subscribe();
        catalogMetrics = AgentCatalogMetrics.register(() -> {
            Diagnostics state = diagnosticsState();
            RemoteCatalogLifecycle.Diagnostics lifecycleState = state.lifecycle();
            return new AgentCatalogMetrics.Snapshot(!closed && !lifecycleState.closed() && lifecycleState.currentVersion() > 0,
                    lifecycleState.updating(), lifecycleState.generations(), lifecycleState.clients(), lifecycleState.references(),
                    lifecycleState.pending(),
                    !state.lastFailure().isEmpty() || !lifecycleState.cleanupFailures().isEmpty());
        });
    }

    /**
     * Create a route-aware managed catalog with default admission.
     * @param allowedRoutes trusted routes
     * @param localNames local tool names
     * @param credentials fixed service credential resolver
     * @return route-aware catalog
     */
    public static ManagedRemoteMcpCatalog forRoutes(final Set<String> allowedRoutes, final Set<String> localNames,
                                                    final RemoteServiceCredentialResolver credentials) {
        return forRoutes(allowedRoutes, localNames, credentials, false);
    }

    /**
     * Create a route-aware managed catalog with default admission.
     * @param allowedRoutes trusted routes
     * @param localNames local tool names
     * @param credentials fixed service credential resolver
     * @param resourcesEnabled whether remote resources are enabled
     * @return route-aware catalog
     */
    public static ManagedRemoteMcpCatalog forRoutes(final Set<String> allowedRoutes, final Set<String> localNames,
                                                    final RemoteServiceCredentialResolver credentials, final boolean resourcesEnabled) {
        return forRoutes(allowedRoutes, localNames, credentials, AgentMcpResourceAdmission.create(AgentGatewayAdmissionConfig.parse(null)), resourcesEnabled);
    }

    /**
     * Create a route-aware managed catalog sharing the gateway admission controller.
     * @param allowedRoutes trusted routes
     * @param localNames local tool names
     * @param credentials fixed service credential resolver
     * @param admission shared controller
     * @param resourcesEnabled whether remote resources are enabled
     * @return route-aware catalog
     */
    public static ManagedRemoteMcpCatalog forRoutes(final Set<String> allowedRoutes, final Set<String> localNames,
                                                    final RemoteServiceCredentialResolver credentials,
                                                    final AgentMcpResourceAdmission.ResourceAdmissionController admission, final boolean resourcesEnabled) {
        return new ManagedRemoteMcpCatalog(allowedRoutes, localNames, credentials, admission, resourcesEnabled, null);
    }

    static ManagedRemoteMcpCatalog forRoutesWithVerificationTlsContext(final Set<String> allowedRoutes, final Set<String> localNames,
                                                                       final RemoteServiceCredentialResolver credentials,
                                                                       final javax.net.ssl.SSLContext verificationTlsContext) {
        return new ManagedRemoteMcpCatalog(allowedRoutes, localNames, credentials,
                AgentMcpResourceAdmission.create(AgentGatewayAdmissionConfig.parse(null)), false, verificationTlsContext);
    }

    private static Set<String> fixedRoutes(final Set<URI> allowedEndpoints) {
        return allowedEndpoints.stream().map(endpoint -> endpoint + "=").collect(java.util.stream.Collectors.toUnmodifiableSet());
    }

    private Mono<Long> applyUpdate(final ControlUpdate update) {
        return Mono.defer(() -> {
            if (update.sequence() != revision.get()) {
                return Mono.empty();
            }
            if (!update.enabled()) {
                return lifecycle.closeIdle().thenReturn(0L);
            }
            return reload(update.source(), () -> !closed && update.sequence() == revision.get()).doOnSuccess(version -> {
                recordFailure(update.sequence(), "");
            }).onErrorResume(error -> {
                recordFailure(update.sequence(), error.getClass().getSimpleName());
                LOG.warn("Agent MCP configuration update rejected ({})", error.getClass().getSimpleName());
                return Mono.empty();
            });
        });
    }

    /**
     * Enqueue an immutable data-sync event without blocking its callback thread.
     * @param plugin authoritative plugin data
     */
    public synchronized void accept(final PluginData plugin) {
        final long sequence = revision.incrementAndGet();
        // Diagnostics belong to the newest event, even if an older update is fenced before reserve().
        lastFailure = "";
        boolean enabled = Boolean.TRUE.equals(plugin.getEnabled());
        if (!enabled) {
            lifecycle.withdraw();
        }
        String source = enabled && Objects.nonNull(plugin.getConfig()) ? plugin.getConfig() : "{}";
        if (source.getBytes(StandardCharsets.UTF_8).length > 16384) {
            lifecycle.withdraw();
            lastFailure = "ConfigurationByteLimit";
            return;
        }
        Sinks.EmitResult result = updates.tryEmitNext(new ControlUpdate(sequence, source, enabled));
        if (result.isFailure() && !closed) {
            lifecycle.withdraw();
            lastFailure = "ConfigurationQueueRejected";
            LOG.warn("Agent MCP configuration queue rejected an update; remote access withdrawn");
        }
    }

    /**
     * Replace configuration through the same validated operation used by data sync.
     * Callers must schedule blocking credential resolution away from event loops.
     * @param source complete trusted plugin config
     * @return published directory generation
     */
    public Mono<Long> reload(final String source) {
        return reload(source, () -> !closed);
    }

    private Mono<Long> reload(final String source, final BooleanSupplier stillCurrent) {
        return Mono.defer(() -> {
            AgentGatewayAggregationConfig config = AgentGatewayAggregationConfig.parsePluginConfig(source);
            List<RemoteCatalogLifecycle.Config> targets = new ArrayList<>();
            for (AgentGatewayAggregationConfig.Server server : config.servers()) {
                RemoteServerBinding.Config reference = new RemoteServerBinding.Config(server.name(), server.endpoint(), server.credentialRef(), server.credentialVersion());
                RemoteServerBinding binding = RemoteServerBinding.resolve(reference, routes, credentials::resolve);
                targets.add(binding.lifecycleConfig(routes.route(server.endpoint()), verificationTlsContext));
            }
            return lifecycle.refresh(targets, List.of(), localNames, stillCurrent)
                    .doOnSuccess(ignored -> {
                        if (stillCurrent.getAsBoolean()) {
                            admission.replace(AgentMcpResourceAdmission.create(config.admission()));
                        }
                    });
        });
    }

    @Override
    public Mono<ObjectNode> withSnapshot(final Function<Snapshot, Mono<ObjectNode>> operation) {
        return Mono.deferContextual(context -> {
            AgentRuntimeMetrics metrics = context.getOrDefault(AgentRuntimeMetrics.KEY, AgentRuntimeMetrics.NOOP);
            java.util.concurrent.atomic.AtomicBoolean acquired = new java.util.concurrent.atomic.AtomicBoolean();
            return lifecycle.withSnapshot(directory -> {
                acquired.set(true);
                AgentRuntimeMetrics.safely(() -> metrics.catalog(true));
                return operation.apply(new CatalogSnapshot(directory));
            }).doOnError(CatalogUnavailableException.class, error -> {
                if (!acquired.get()) {
                    AgentRuntimeMetrics.safely(() -> metrics.catalog(false));
                }
            });
        });
    }

    @Override
    public AgentMcpResourceAdmission admission() {
        return admission;
    }

    /**
     * Current validated admission limits.
     * @return immutable trusted configuration
     */
    public AgentGatewayAdmissionConfig admissionConfig() {
        return admission.config();
    }

    /**
     * Secret-free lifecycle diagnostics for management and verification.
     * @return immutable diagnostic values
     */
    public Map<String, Object> diagnostics() {
        Diagnostics state = diagnosticsState();
        Map<String, Object> result = new LinkedHashMap<>(state.lifecycle().toMap());
        result.put("lastFailure", state.lastFailure());
        AgentGatewayAdmissionConfig config = admission.config();
        result.put("admission", Map.of("globalInFlight", config.globalInFlight(), "globalQueued", config.globalQueued(),
                "localInFlight", config.localInFlight(), "localQueued", config.localQueued(),
                "remoteInFlight", config.remoteInFlight(), "remoteQueued", config.remoteQueued()));
        return Map.copyOf(result);
    }

    /**
     * Capture typed lifecycle and current control-update failure diagnostics.
     * @return immutable control-plane diagnostic snapshot
     */
    public synchronized Diagnostics diagnosticsState() {
        return new Diagnostics(lifecycle.diagnosticsState(), revision.get(), lastFailure);
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
            closed = true;
            catalogMetrics.close();
            admission.close();
            worker.dispose();
            scheduler.dispose();
            return lifecycle.closeGracefully();
        });
    }

    @Override
    public void close() {
        closeAsync().block(Duration.ofSeconds(8));
    }

    /** Failure is scoped to revision; empty means no failure recorded, not a ready catalog. */
    public record Diagnostics(RemoteCatalogLifecycle.Diagnostics lifecycle, long revision, String lastFailure) {
    }

    private record ControlUpdate(long sequence, String source, boolean enabled) {
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
                .contextWrite(value -> value.put(McpTransportContext.KEY, McpTransportContext.create(Map.of(
                        "subject", context.getSubject(), "deadline", context.getDeadline(),
                        AgentTraceContext.HEADERS_KEY, AgentTraceContext.headers(value.getOrDefault(AgentTraceContext.HEADERS_KEY, null))))));
        }
    }
}
