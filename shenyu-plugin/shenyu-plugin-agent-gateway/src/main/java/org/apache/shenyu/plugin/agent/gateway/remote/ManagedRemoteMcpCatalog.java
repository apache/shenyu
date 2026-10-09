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
import java.net.URI;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.function.Function;
import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

/** Managed, opt-in catalog fed exclusively by the trusted plugin data-sync channel. */
public final class ManagedRemoteMcpCatalog implements AgentMcpRemoteCatalog, AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(ManagedRemoteMcpCatalog.class);

    private static final ObjectMapper JSON = new ObjectMapper();

    private final RemoteCatalogLifecycle lifecycle = new RemoteCatalogLifecycle(3);

    private final Set<URI> allowedEndpoints;

    private final Set<String> localNames;

    private final RemoteServiceCredentialResolver credentials;

    private final Scheduler scheduler = Schedulers.newBoundedElastic(1, 16, "agent-mcp-config");

    private final Sinks.Many<ControlUpdate> updates = Sinks.many().unicast().onBackpressureBuffer(new ArrayBlockingQueue<>(16));

    private final java.util.concurrent.atomic.AtomicLong revision = new java.util.concurrent.atomic.AtomicLong();

    private final Disposable worker;

    private volatile String lastFailure = "";

    private volatile boolean closed;

    /**
     * Managed Remote Mcp Catalog.
     * @param allowedEndpoints trusted allowedEndpoints value
     * @param localNames trusted localNames value
     * @param credentials trusted credentials value
     */
    public ManagedRemoteMcpCatalog(final Set<URI> allowedEndpoints, final Set<String> localNames, final RemoteServiceCredentialResolver credentials) {
        this.allowedEndpoints = Set.copyOf(allowedEndpoints);
        this.localNames = Set.copyOf(localNames);
        this.credentials = java.util.Objects.requireNonNull(credentials, "credentials");
        this.allowedEndpoints.forEach(RemoteTransportPolicy::endpoint);
        // The single owned subscription belongs to the control-plane lifecycle, never a request.
        lifecycle.refresh(List.of()).block(Duration.ofSeconds(6));
        worker = updates.asFlux().publishOn(scheduler, 1).concatMap(this::applyUpdate, 1).subscribe();
    }

    private Mono<Long> applyUpdate(final ControlUpdate update) {
        return Mono.defer(() -> {
            if (update.sequence() != revision.get()) {
                return Mono.empty();
            }
            return reload(update.source(), () -> !closed && update.sequence() == revision.get()).doOnSuccess(version -> {
                if (java.util.Objects.nonNull(version) && update.sequence() == revision.get()) {
                    lastFailure = "";
                }
            }).onErrorResume(error -> {
                if (update.sequence() == revision.get()) {
                    lastFailure = error.getClass().getSimpleName();
                }
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
        long sequence = revision.incrementAndGet();
        boolean enabled = Boolean.TRUE.equals(plugin.getEnabled());
        if (!enabled) {
            lifecycle.withdraw();
        }
        String source = enabled && java.util.Objects.nonNull(plugin.getConfig()) ? plugin.getConfig() : "{}";
        if (source.getBytes(java.nio.charset.StandardCharsets.UTF_8).length > 16384) {
            lifecycle.withdraw();
            lastFailure = "ConfigurationByteLimit";
            return;
        }
        Sinks.EmitResult result = updates.tryEmitNext(new ControlUpdate(sequence, source));
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

    private Mono<Long> reload(final String source, final java.util.function.BooleanSupplier valid) {
        return Mono.defer(() -> {
            AgentGatewayAggregationConfig config = AgentGatewayAggregationConfig.parsePluginConfig(source);
            List<RemoteCatalogLifecycle.Config> targets = new ArrayList<>();
            for (AgentGatewayAggregationConfig.Server server : config.servers()) {
                RemoteServerBinding.Config reference = new RemoteServerBinding.Config(server.name(), server.endpoint(), server.credentialRef(), server.credentialVersion());
                targets.add(RemoteServerBinding.resolve(reference, allowedEndpoints, credentials::resolve).lifecycleConfig());
            }
            return lifecycle.refresh(targets, List.of(), localNames, valid);
        });
    }

    @Override
    public Mono<ObjectNode> withSnapshot(final Function<Snapshot, Mono<ObjectNode>> operation) {
        return lifecycle.withSnapshot(directory -> operation.apply(new CatalogSnapshot(directory)));
    }

    /**
     * Secret-free lifecycle diagnostics for management and verification.
     * @return immutable diagnostic values
     */
    public Map<String, Object> diagnostics() {
        Map<String, Object> result = new LinkedHashMap<>(lifecycle.diagnostics());
        result.put("lastFailure", lastFailure);
        return Map.copyOf(result);
    }

    /**
     * Stop owned configuration work and drain the catalog before process exit.
     * @return cleanup completion, or explicit cleanup failure
     */
    public Mono<Void> closeAsync() {
        return Mono.defer(() -> {
            closed = true;
            worker.dispose();
            scheduler.dispose();
            return lifecycle.closeGracefully();
        });
    }

    @Override
    public void close() {
        closeAsync().block(Duration.ofSeconds(8));
    }

    private record ControlUpdate(long sequence, String source) {
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
