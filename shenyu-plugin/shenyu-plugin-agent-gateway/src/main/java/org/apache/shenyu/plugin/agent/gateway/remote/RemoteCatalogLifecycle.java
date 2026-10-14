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
import io.modelcontextprotocol.spec.McpSchema;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog.CatalogUnavailableException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import java.util.function.Supplier;

/** Owns complete directory generations, operation leases and bounded session cleanup. */
public final class RemoteCatalogLifecycle {

    private final Object lock = new Object();

    private final Set<Generation> generations = Collections.newSetFromMap(new IdentityHashMap<>());

    private final Map<RemoteMcpEndpoint, Generation> owners = new IdentityHashMap<>();

    private final Sinks.Empty<Void> drained = Sinks.empty();

    private final List<String> cleanupFailures = new ArrayList<>();

    private final int maxGenerations;

    private Generation current;

    private Generation updatingGeneration;

    private boolean closed;

    private long sequence;

    /**
     * Remote Catalog Lifecycle.
     * @param maxGenerations trusted maxGenerations value
     */
    public RemoteCatalogLifecycle(final int maxGenerations) {
        if (maxGenerations < 2 || maxGenerations > 16) {
            throw new IllegalArgumentException("Generation budget");
        }
        this.maxGenerations = maxGenerations;
    }

    /**
     * refresh.
     * @param source trusted source value
     * @return operation result
     */
    public Mono<Long> refresh(final List<Config> source) {
        return refresh(source, List.of());
    }

    /**
     * refresh.
     * @param source trusted source value
     * @param locals trusted locals value
     * @return operation result
     */
    public Mono<Long> refresh(final List<Config> source, final List<RemoteToolDirectory.LocalTool> locals) {
        return refresh(source, locals, Set.of());
    }

    /**
     * Publish only a complete directory disjoint from startup-local names.
     * @param source trusted remote targets
     * @param locals internal native-result providers
     * @param reservedNames names owned by the existing local registry
     * @return published generation
     */
    public Mono<Long> refresh(final List<Config> source, final List<RemoteToolDirectory.LocalTool> locals, final Set<String> reservedNames) {
        return refresh(source, locals, reservedNames, () -> true);
    }

    Mono<Long> refresh(final List<Config> source, final List<RemoteToolDirectory.LocalTool> locals, final Set<String> reservedNames,
                       final BooleanSupplier stillCurrent) {
        List<Config> configs = List.copyOf(source);
        List<RemoteToolDirectory.LocalTool> localSnapshot = List.copyOf(locals);
        Set<String> reserved = Set.copyOf(reservedNames);
        if (configs.size() > 8 || configs.stream().map(Config::name).distinct().count() != configs.size()) {
            return Mono.error(new IllegalArgumentException("Invalid or duplicate target configuration"));
        }
        return Mono.usingWhen(
            Mono.defer(() -> {
                Generation candidate = reserve(stillCurrent);
                // A fenced control update has no resource and must not enter the build/cleanup paths.
                return Objects.isNull(candidate) ? Mono.empty() : Mono.just(candidate);
            }),
            candidate ->
                closeIdle()
                    .then(build(candidate, configs))
                    .map(directory -> directory.withLocal(localSnapshot, 16))
                    .map(directory -> {
                        if (directory.names().stream().anyMatch(reserved::contains)) {
                            throw new IllegalArgumentException("Local/remote namespace collision");
                        }
                        return directory;
                    })
                    .timeout(Duration.ofSeconds(5))
                    .map(directory -> {
                        synchronized (lock) {
                            if (closed || candidate.retired || !stillCurrent.getAsBoolean() || !cleanupFailures.isEmpty()) {
                                throw new CatalogUnavailableException();
                            }
                            candidate.directory = directory;
                            current = candidate;
                            return candidate.version;
                        }
                    }),
            this::finishUpdate,
            (candidate, error) -> abandon(candidate),
            this::abandon
        );
    }

    private Generation reserve(final BooleanSupplier stillCurrent) {
        synchronized (lock) {
            // Recheck after credential resolution, atomically with generation allocation.
            if (!stillCurrent.getAsBoolean()) {
                return null;
            }
            if (closed || Objects.nonNull(updatingGeneration) || !cleanupFailures.isEmpty() || generations.size() >= maxGenerations) {
                throw new CatalogUnavailableException();
            }
            Generation candidate = new Generation(++sequence);
            generations.add(candidate);
            updatingGeneration = candidate;
            // Fail closed for new requests throughout a controlled configuration replacement.
            if (Objects.nonNull(current)) {
                current.retired = true;
            }
            current = null;
            return candidate;
        }
    }

    private Mono<RemoteToolDirectory> build(final Generation candidate, final List<Config> configs) {
        Objects.requireNonNull(candidate, "A reserved generation is required to build a directory");
        return Flux.fromIterable(configs)
            .concatMap(config ->
                Mono.defer(() -> {
                    RemoteMcpEndpoint client;
                    synchronized (lock) {
                        if (closed || candidate.retired || !generations.contains(candidate) || !cleanupFailures.isEmpty()) {
                            return Mono.error(new CatalogUnavailableException());
                        }
                        // Managed factories are local, non-blocking constructors; allocation and ownership are atomic.
                        client = config.create().get();
                        // Factories must return exclusively owned clients; never borrow another generation's client.
                        if (Objects.isNull(client)) {
                            return Mono.error(new IllegalStateException("Null target client"));
                        }
                        if (owners.containsKey(client)) {
                            return Mono.error(new IllegalStateException("Client already owned"));
                        }
                        candidate.clients.add(client);
                        owners.put(client, candidate);
                    }
                    return client
                        .initialize()
                        .switchIfEmpty(Mono.error(new IllegalStateException("Empty handshake")))
                        .map(ignored -> new RemoteToolDirectory.Target(config.name(), client::listTools, client::callTool, client::callRaw));
                })
            )
            .collectList()
            .flatMap(targets -> targets.isEmpty() ? Mono.just(RemoteToolDirectory.empty()) : RemoteToolDirectory.discover(targets, 4, 16));
    }

    private Mono<Void> finishUpdate(final Generation candidate) {
        return closeIdle()
            .doOnTerminate(() -> endUpdate(candidate))
            .doFinally(ignored -> endUpdate(candidate));
    }

    private Mono<Void> abandon(final Generation candidate) {
        synchronized (lock) {
            candidate.retired = true;
            if (current == candidate) {
                current = null;
            }
        }
        return closeIdle()
            .doOnTerminate(() -> endUpdate(candidate))
            .doFinally(ignored -> endUpdate(candidate));
    }

    private void endUpdate(final Generation candidate) {
        synchronized (lock) {
            // An older final callback must never clear a newer update's reservation.
            if (updatingGeneration == candidate) {
                updatingGeneration = null;
            }
            signalDrain();
        }
    }

    /**
     * call.
     * @param name trusted name value
     * @param rule trusted rule value
     * @param grants trusted grants value
     * @param arguments trusted arguments value
     * @return operation result
     */
    public Mono<McpSchema.CallToolResult> call(final String name, final Set<String> rule, final Set<String> grants, final Map<String, Object> arguments) {
        Set<String> ruleSnapshot = Set.copyOf(rule);
        Set<String> grantsSnapshot = Set.copyOf(grants);
        return withArguments(arguments, (directory, input) -> directory.call(name, ruleSnapshot, grantsSnapshot, input));
    }

    private Generation acquire() {
        synchronized (lock) {
            if (closed || Objects.isNull(current)) {
                throw new CatalogUnavailableException();
            }
            current.references++;
            return current;
        }
    }

    /** Stop new remote access immediately while asynchronous retirement proceeds. */
    public void withdraw() {
        synchronized (lock) {
            if (Objects.nonNull(updatingGeneration)) {
                updatingGeneration.retired = true;
            }
            if (Objects.nonNull(current)) {
                current.retired = true;
                current = null;
            }
        }
    }

    /**
     * call Raw.
     * @param name trusted name value
     * @param rule trusted rule value
     * @param grants trusted grants value
     * @param arguments trusted arguments value
     * @return operation result
     */
    public Mono<ObjectNode> callRaw(final String name, final Set<String> rule, final Set<String> grants, final Map<String, Object> arguments) {
        Set<String> ruleSnapshot = Set.copyOf(rule);
        Set<String> grantsSnapshot = Set.copyOf(grants);
        return withArguments(arguments, (directory, input) -> directory.callRaw(name, ruleSnapshot, grantsSnapshot, input));
    }

    private <T> Mono<T> withArguments(final Map<String, Object> arguments,
                                    final BiFunction<RemoteToolDirectory, Map<String, Object>, Mono<T>> operation) {
        // Own nested input at assembly; each subscription receives a fresh copy and a directory lease.
        var json = new ObjectMapper();
        var input = json.valueToTree(arguments);
        return withSnapshot(directory -> operation.apply(directory, json.convertValue(input.deepCopy(), new TypeReference<Map<String, Object>>() { })));
    }

    private Mono<Void> release(final Generation generation) {
        return Mono.defer(() -> {
            synchronized (lock) {
                if (--generation.references < 0) {
                    throw new IllegalStateException("Double lease release");
                }
            }
            return closeIdle();
        });
    }

    private Mono<Void> closeIdle() {
        List<Mono<Void>> tasks = new ArrayList<>();
        synchronized (lock) {
            for (Generation generation : generations) {
                if (generation.retired && generation.references == 0) {
                    if (Objects.isNull(generation.cleanup)) {
                        List<RemoteMcpEndpoint> clients = List.copyOf(generation.clients);
                        generation.cleanup = Flux.fromIterable(clients)
                            .flatMap(
                                client ->
                                    Mono.defer(client::closeGracefully)
                                        .timeout(Duration.ofSeconds(2))
                                        .onErrorResume(error -> {
                                            synchronized (lock) {
                                                generation.cleanupFailed = true;
                                                cleanupFailures.add("generation " + generation.version + ": " + error.getClass().getSimpleName());
                                            }
                                            return Mono.empty();
                                        }),
                                8
                            )
                            .then()
                            .doOnTerminate(() -> {
                                synchronized (lock) {
                                    // Failed closes stay quarantined and consume budget, never silently reused.
                                    if (!generation.cleanupFailed) {
                                        generations.remove(generation);
                                        for (RemoteMcpEndpoint client : clients) {
                                            owners.remove(client);
                                        }
                                    }
                                    signalDrain();
                                }
                            })
                            .cache();
                    }
                    tasks.add(generation.cleanup);
                }
            }
        }
        return Mono.when(tasks);
    }

    /**
     * visible.
     * @param rule trusted rule value
     * @param grants trusted grants value
     * @return operation result
     */
    public Set<String> visible(final Set<String> rule, final Set<String> grants) {
        synchronized (lock) {
            if (closed || Objects.isNull(current)) {
                throw new CatalogUnavailableException();
            }
            return current.directory.visible(rule, grants);
        }
    }

    /**
     * diagnostics.
     * @return operation result
     */
    public Map<String, Object> diagnostics() {
        return diagnosticsState().toMap();
    }

    /**
     * Return an immutable, typed lifecycle snapshot; no request data or credentials.
     * @return lifecycle state captured under the generation lock
     */
    public Diagnostics diagnosticsState() {
        synchronized (lock) {
            return new Diagnostics(closed, Objects.nonNull(updatingGeneration), generations.size(), owners.size(),
                    generations.stream().mapToInt(g -> g.references).sum(), Objects.isNull(current) ? 0L : current.version, cleanupFailures);
        }
    }

    /**
     * definitions.
     * @param rule trusted rule value
     * @param grants trusted grants value
     * @return operation result
     */
    public Map<String, McpSchema.Tool> definitions(final Set<String> rule, final Set<String> grants) {
        synchronized (lock) {
            if (closed || Objects.isNull(current)) {
                throw new CatalogUnavailableException();
            }
            return current.directory.definitions(rule, grants);
        }
    }

    /**
     * Keep one generation leased across a whole discover/list/call operation.
     * @param <T> operation result type
     * @param operation trusted operation value
     * @return operation result
     */
    public <T> Mono<T> withSnapshot(final Function<RemoteToolDirectory, Mono<T>> operation) {
        return Mono.usingWhen(
            Mono.fromCallable(this::acquire),
            generation -> operation.apply(generation.directory),
            this::release,
            (generation, error) -> release(generation),
            this::release
        );
    }

    /**
     * close Gracefully.
     * @return operation result
     */
    public Mono<Void> closeGracefully() {
        return Mono.defer(() -> {
            synchronized (lock) {
                closed = true;
                if (Objects.nonNull(current)) {
                    current.retired = true;
                }
                current = null;
                signalDrain();
            }
            return closeIdle()
                .then(
                    Mono.defer(() -> {
                        synchronized (lock) {
                            return cleanupFailures.isEmpty() ? drained.asMono() : Mono.error(new IllegalStateException("Client cleanup failed"));
                        }
                    })
                )
                .timeout(Duration.ofSeconds(6));
        });
    }

    private void signalDrain() {
        if (closed && Objects.isNull(updatingGeneration) && generations.isEmpty()) {
            drained.tryEmitEmpty();
        }
    }

    /** Immutable lifecycle diagnostics; map export is retained for existing management consumers. */
    public record Diagnostics(boolean closed, boolean updating, int generations, int clients, int references,
                              long currentVersion, List<String> cleanupFailures) {
        public Diagnostics {
            cleanupFailures = List.copyOf(cleanupFailures);
        }

        /**
         * Preserve the existing secret-free management export without changing the typed snapshot.
         * @return immutable legacy field map
         */
        public Map<String, Object> toMap() {
            return Map.of("closed", closed, "updating", updating, "generations", generations, "clients", clients,
                    "references", references, "currentVersion", currentVersion, "cleanupFailures", cleanupFailures);
        }
    }

    public record Config(String name, String credentialVersion, Supplier<RemoteMcpEndpoint> create) {
        public Config {
            if (
                Objects.isNull(name)
                    || !name.matches("[A-Za-z0-9_-]{1,48}")
                    || Objects.isNull(credentialVersion)
                    || credentialVersion.isBlank()
                    || Objects.isNull(create)
            ) {
                throw new IllegalArgumentException("Invalid controlled target configuration");
            }
        }
    }

    private static final class Generation {

        private final long version;

        private final List<RemoteMcpEndpoint> clients = new ArrayList<>();

        private RemoteToolDirectory directory;

        private int references;

        private boolean retired;

        private boolean cleanupFailed;

        private Mono<Void> cleanup;

        Generation(final long version) {
            this.version = version;
        }
    }
}
