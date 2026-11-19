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

package org.apache.shenyu.plugin.mcp.server.transport;

import io.modelcontextprotocol.spec.McpServerSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.LongSupplier;
import java.util.function.Supplier;

/** Single owner for admission, activity leases and terminal session cleanup. */
final class McpSessionLifecycle implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(McpSessionLifecycle.class);

    private final McpSessionPolicy policy;

    private final Consumer<String> cleanup;

    private final LongSupplier clock;

    private final Map<String, Entry> entries = new HashMap<>();

    private final Disposable sweep;

    private final Mono<Void> gracefulClose = Mono.defer(this::drainAndClose).cache();

    private boolean closed;

    /**
     * Create an owner using the process monotonic clock and shared expiry scheduler.
     *
     * @param policy validated startup limits
     * @param cleanup local provider state removal
     */
    McpSessionLifecycle(final McpSessionPolicy policy, final Consumer<String> cleanup) {
        this(policy, cleanup, System::nanoTime, Schedulers.parallel());
    }

    /**
     * Create an owner with injectable monotonic time and scheduling.
     *
     * @param policy validated startup limits
     * @param cleanup local provider state removal
     * @param clock monotonic nanosecond clock
     * @param scheduler expiry scheduler
     */
    McpSessionLifecycle(final McpSessionPolicy policy, final Consumer<String> cleanup, final LongSupplier clock, final Scheduler scheduler) {
        this.policy = Objects.requireNonNull(policy);
        this.cleanup = cleanup;
        this.clock = clock;
        final long interval = policy.sweepInterval().toNanos();
        this.sweep = policy.idleTimeout().isZero() ? null : scheduler.schedulePeriodically(this::expire, interval, interval, TimeUnit.NANOSECONDS);
    }

    /**
     * Admit and install a session atomically, closing a failed installation outside the monitor.
     *
     * @param factory session factory
     * @param install provider index registration
     * @return admitted session
     */
    McpServerSession create(final Supplier<McpServerSession> factory, final Consumer<McpServerSession> install) {
        Entry failed = null;
        try {
            synchronized (this) {
                if (closed || policy.maxSessions() > 0 && entries.size() >= policy.maxSessions()) {
                    throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "MCP session capacity unavailable");
                }
                final McpServerSession session = factory.get();
                final Entry entry = new Entry(session, clock.getAsLong());
                if (Objects.nonNull(entries.putIfAbsent(session.getId(), entry))) {
                    throw new IllegalStateException("Duplicate MCP session id");
                }
                try {
                    install.accept(session);
                } catch (RuntimeException error) {
                    entries.remove(session.getId());
                    failed = entry;
                    throw error;
                }
                return session;
            }
        } catch (RuntimeException error) {
            if (Objects.nonNull(failed)) {
                terminate(failed);
            }
            throw error;
        }
    }

    /**
     * Hold an activity lease through terminal completion, error or cancellation.
     *
     * @param id session identifier
     * @param operation request publisher factory
     * @param <T> response type
     * @return request result
     */
    <T> Mono<T> withActivity(final String id, final Supplier<Mono<T>> operation) {
        return Mono.defer(() -> {
            final Entry entry;
            final Mono<T> work;
            synchronized (this) {
                if (closed) {
                    return Mono.error(new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "MCP server is shutting down"));
                }
                entry = entries.get(id);
                if (Objects.isNull(entry)) {
                    return Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "MCP session is closed"));
                }
                entry.active++;
                try {
                    work = operation.get();
                } catch (RuntimeException error) {
                    release(entry);
                    return Mono.error(error);
                }
            }
            final Mono<T> stopped = entry.stopped.asMono().then(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "MCP session is closed")));
            return Mono.firstWithSignal(stopped, work).doFinally(signal -> release(entry));
        });
    }

    private void release(final Entry entry) {
        final boolean drained;
        synchronized (this) {
            entry.active--;
            entry.lastActivity = clock.getAsLong();
            drained = closed && entry.active == 0;
        }
        if (drained) {
            entry.drained.tryEmitEmpty();
        }
    }

    /**
     * Bind an exchange while the session can still accept work.
     *
     * @param id session identifier
     * @param binding local exchange registration
     * @return whether the binding was accepted
     */
    synchronized boolean bind(final String id, final Runnable binding) {
        if (closed || !entries.containsKey(id)) {
            return false;
        }
        binding.run();
        return true;
    }

    /**
     * Claim and close a session once; repeated removals have no effect.
     *
     * @param id session identifier
     */
    void remove(final String id) {
        final Entry entry;
        synchronized (this) {
            final Entry current = entries.get(id);
            if (Objects.nonNull(current) && current.gracefulClosing) {
                return;
            }
            entry = entries.remove(id);
        }
        if (Objects.nonNull(entry)) {
            terminate(entry);
        }
    }

    private void expire() {
        final List<Entry> expired = new ArrayList<>();
        synchronized (this) {
            if (closed) {
                return;
            }
            final long now = clock.getAsLong();
            entries.values().removeIf(entry -> {
                if (entry.active == 0 && now - entry.lastActivity >= policy.idleTimeout().toNanos()) {
                    expired.add(entry);
                    return true;
                }
                return false;
            });
        }
        expired.forEach(this::terminate);
    }

    private void cleanup(final Entry entry) {
        try {
            cleanup.accept(entry.session.getId());
        } catch (RuntimeException error) {
            LOG.warn("MCP session local cleanup failed", error);
        }
    }

    private void terminate(final Entry entry) {
        try {
            cleanup(entry);
            entry.session.close();
        } catch (RuntimeException error) {
            LOG.warn("MCP session close failed after local removal", error);
        } finally {
            entry.stopped.tryEmitEmpty();
        }
    }

    /**
     * Stop accepting work, drain active operations and await SDK closure of every session.
     *
     * @return shared shutdown completion
     */
    Mono<Void> closeGracefully() {
        return gracefulClose;
    }

    private Mono<Void> drainAndClose() {
        final List<Entry> remaining;
        synchronized (this) {
            closed = true;
            remaining = List.copyOf(entries.values());
        }
        disposeSweep();
        return Mono.whenDelayError(remaining.stream().map(this::drain).toList());
    }

    private Mono<Void> drain(final Entry entry) {
        final Mono<Void> ready;
        synchronized (this) {
            ready = entry.active == 0 ? Mono.empty() : Mono.firstWithSignal(entry.drained.asMono(), entry.stopped.asMono());
        }
        return ready.then(Mono.defer(() -> finishGracefully(entry)));
    }

    private Mono<Void> finishGracefully(final Entry entry) {
        synchronized (this) {
            if (entries.get(entry.session.getId()) != entry) {
                return entry.stopped.asMono();
            }
            entry.gracefulClosing = true;
        }
        return Mono.firstWithSignal(entry.stopped.asMono(), Mono.defer(entry.session::closeGracefully))
                .then(Mono.<Void>fromRunnable(() -> {
                    if (claim(entry)) {
                        cleanup(entry);
                        entry.stopped.tryEmitEmpty();
                    }
                }))
                .onErrorResume(error -> {
                    if (claim(entry)) {
                        terminate(entry);
                    }
                    return Mono.error(error);
                });
    }

    private synchronized boolean claim(final Entry entry) {
        return entries.remove(entry.session.getId(), entry);
    }

    /** Stop admission and immediately cancel remaining session operations. */
    @Override
    public void close() {
        final List<Entry> remaining;
        synchronized (this) {
            closed = true;
            remaining = new ArrayList<>(entries.values());
            entries.clear();
        }
        disposeSweep();
        remaining.forEach(this::terminate);
    }

    private void disposeSweep() {
        if (Objects.nonNull(sweep)) {
            sweep.dispose();
        }
    }

    private static final class Entry {

        private final McpServerSession session;

        private final Sinks.Empty<Void> stopped = Sinks.empty();

        private final Sinks.Empty<Void> drained = Sinks.empty();

        private long lastActivity;

        private int active;

        private boolean gracefulClosing;

        private Entry(final McpServerSession session, final long lastActivity) {
            this.session = session;
            this.lastActivity = lastActivity;
        }
    }
}
