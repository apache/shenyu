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

    private boolean closed;

    McpSessionLifecycle(final McpSessionPolicy policy, final Consumer<String> cleanup) {
        this(policy, cleanup, System::nanoTime, Schedulers.parallel());
    }

    McpSessionLifecycle(final McpSessionPolicy policy, final Consumer<String> cleanup, final LongSupplier clock, final Scheduler scheduler) {
        this.policy = Objects.requireNonNull(policy);
        this.cleanup = cleanup;
        this.clock = clock;
        final long interval = policy.sweepInterval().toNanos();
        this.sweep = policy.idleTimeout().isZero() ? null : scheduler.schedulePeriodically(this::expire, interval, interval, TimeUnit.NANOSECONDS);
    }

    synchronized McpServerSession create(final Supplier<McpServerSession> factory, final Consumer<McpServerSession> install) {
        if (closed || policy.maxSessions() > 0 && entries.size() >= policy.maxSessions()) {
            throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "MCP session capacity unavailable");
        }
        final McpServerSession session = factory.get();
        if (entries.containsKey(session.getId())) {
            throw new IllegalStateException("Duplicate MCP session id");
        }
        final Entry entry = new Entry(session, clock.getAsLong());
        entries.put(session.getId(), entry);
        try {
            install.accept(session);
            return session;
        } catch (RuntimeException error) {
            entries.remove(session.getId());
            terminate(entry);
            throw error;
        }
    }

    <T> Mono<T> withActivity(final String id, final Supplier<Mono<T>> operation) {
        return Mono.defer(() -> {
            final Entry entry;
            final Mono<T> work;
            synchronized (this) {
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

    private synchronized void release(final Entry entry) {
        entry.active--;
        entry.lastActivity = clock.getAsLong();
    }

    synchronized boolean bind(final String id, final Runnable binding) {
        if (!entries.containsKey(id)) {
            return false;
        }
        binding.run();
        return true;
    }

    void remove(final String id) {
        final Entry entry;
        synchronized (this) {
            entry = entries.remove(id);
        }
        if (Objects.nonNull(entry)) {
            terminate(entry);
        }
    }

    private void expire() {
        final List<Entry> expired = new ArrayList<>();
        synchronized (this) {
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

    private void terminate(final Entry entry) {
        try {
            cleanup.accept(entry.session.getId());
        } catch (RuntimeException error) {
            LOG.warn("MCP session local cleanup failed", error);
        } finally {
            entry.stopped.tryEmitEmpty();
        }
        try {
            entry.session.close();
        } catch (RuntimeException error) {
            LOG.warn("MCP session close failed after local removal", error);
        }
    }

    @Override
    public void close() {
        final List<Entry> remaining;
        synchronized (this) {
            closed = true;
            remaining = new ArrayList<>(entries.values());
            entries.clear();
        }
        if (Objects.nonNull(sweep)) {
            sweep.dispose();
        }
        remaining.forEach(this::terminate);
    }

    private static final class Entry {

        private final McpServerSession session;

        private final Sinks.Empty<Void> stopped = Sinks.empty();

        private long lastActivity;

        private int active;

        private Entry(final McpServerSession session, final long lastActivity) {
            this.session = session;
            this.lastActivity = lastActivity;
        }
    }
}
