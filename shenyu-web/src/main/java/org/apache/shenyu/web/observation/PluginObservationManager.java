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

package org.apache.shenyu.web.observation;

import org.apache.shenyu.common.metrics.PluginObservationEvent;
import org.apache.shenyu.common.metrics.PluginObservationSink;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.observation.PluginObservationContext;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;

/** Optional sampled bounded delivery. Exporters never run on request/EventLoop threads. */
public final class PluginObservationManager implements AutoCloseable {

    private static final PluginObservationManager DISABLED = new PluginObservationManager();

    private static final Identity OTHER = new Identity("other", "UNKNOWN");

    private final boolean enabled;

    private final int sampleEvery;

    private final int eventLimit;

    private final ArrayBlockingQueue<PluginObservationEvent> queue;

    private final PluginObservationSink sink;

    private final Thread worker;

    private final AtomicBoolean closed = new AtomicBoolean();

    private final AtomicLong requests = new AtomicLong();

    private final Map<String, Identity> identities = new ConcurrentHashMap<>();

    private final LongAdder queueDrops = new LongAdder();

    private final LongAdder budgetDrops = new LongAdder();

    private final LongAdder consumerFailures = new LongAdder();

    private PluginObservationManager() {
        enabled = false;
        sampleEvery = 1;
        eventLimit = 0;
        queue = null;
        sink = PluginObservationSink.NOOP;
        worker = null;
    }

    /**
     * Configure a startup-owned worker and finite per-request/queue allowances.
     * @param capacity queue entries, 1..8192
     * @param sampleEvery sample one of every N root subscriptions
     * @param maxRecords per-root records including a reserved truncation record
     * @param maxBytes conservative per-root export bound (8192 bytes per bounded event)
     * @param sink optional existing metrics/tracing adapter
     */
    public PluginObservationManager(final int capacity, final int sampleEvery, final int maxRecords, final int maxBytes,
                                    final PluginObservationSink sink) {
        if (capacity < 1 || capacity > 8192 || sampleEvery < 1 || maxRecords < 2 || maxRecords > 4096
                || maxBytes < 16384 || maxBytes > 4194304) {
            throw new IllegalArgumentException("Invalid plugin observation bounds");
        }
        this.enabled = true;
        this.sampleEvery = sampleEvery;
        eventLimit = Math.min(maxRecords, maxBytes / 8192);
        queue = new ArrayBlockingQueue<>(capacity);
        this.sink = Objects.requireNonNull(sink);
        worker = new Thread(this::consume, "shenyu-plugin-observer");
        worker.setDaemon(true);
        worker.setContextClassLoader(null);
        PluginObservationContext.activate();
        worker.start();
    }

    /**
     * Default-disabled singleton allocates no queue, scope, worker or event.
     * @return no-op manager
     */
    public static PluginObservationManager disabled() {
        return DISABLED;
    }

    /**
     * Freeze at most 63 implementation labels at configuration/setup, never by request data.
     * @param plugins installed/updated implementation objects, not retained by this manager
     */
    public synchronized void prepare(final List<ShenyuPlugin> plugins) {
        if (!enabled) {
            return;
        }
        for (ShenyuPlugin plugin : plugins) {
            if (identities.size() >= 63) {
                return;
            }
            String type = plugin.getClass().getName();
            String label = plugin.getClass().getSimpleName();
            identities.putIfAbsent(type, new Identity(PluginObservationEvent.bounded(label.isEmpty() ? "anonymous" : label, 64),
                    PluginObservationEvent.bounded(type, 256)));
        }
    }

    /**
     * Sample at the actual root subscription, not while the handler publisher is assembled.
     * @return scope, or null on the disabled/unsampled path
     */
    public Scope start() {
        if (!enabled || closed.get()) {
            return null;
        }
        long request = requests.incrementAndGet();
        return (request - 1) % sampleEvery == 0 ? new Scope(request) : null;
    }

    /**
     * Bounded delivery state, including drops that cannot enter a saturated queue.
     * @return immutable counters
     */
    public Stats stats() {
        return new Stats(enabled, enabled ? queue.size() : 0, queueDrops.sum(), budgetDrops.sum(), consumerFailures.sum());
    }

    private void offer(final PluginObservationEvent event) {
        if (closed.get() || !queue.offer(event)) {
            queueDrops.increment();
        }
    }

    private void consume() {
        while (!closed.get()) {
            try {
                PluginObservationEvent event = queue.take();
                try {
                    sink.accept(event);
                } catch (RuntimeException | AssertionError error) {
                    // Never log callback messages or attach them to request failures.
                    consumerFailures.increment();
                }
            } catch (InterruptedException interrupted) {
                if (closed.get()) {
                    return;
                }
            }
        }
    }

    @Override
    public void close() {
        if (enabled && closed.compareAndSet(false, true)) {
            PluginObservationContext.deactivate();
            queue.clear();
            worker.interrupt();
        }
    }

    private static PluginObservationEvent.Outcome outcome(final SignalType signal) {
        if (signal == SignalType.CANCEL) {
            return PluginObservationEvent.Outcome.CANCELLED;
        }
        return signal == SignalType.ON_ERROR ? PluginObservationEvent.Outcome.ERROR : PluginObservationEvent.Outcome.COMPLETE;
    }

    /** Local sampled-root identity, shared by supported fork dispatch. */
    public final class Scope {

        private final long request;

        private final AtomicLong sequence = new AtomicLong();

        private final AtomicInteger records = new AtomicInteger();

        private Scope(final long request) {
            this.request = request;
        }

        /**
         * Create the identity of one actual plugin invocation.
         * @param plugin implementation, not retained by the span
         * @param trace existing trace correlation, not a metric label
         * @return invocation state
         */
        public Span span(final ShenyuPlugin plugin, final String trace) {
            return new Span(this, sequence.incrementAndGet(), identities.getOrDefault(plugin.getClass().getName(), OTHER),
                    PluginObservationEvent.bounded(trace, 128));
        }

        private void record(final Span span, final long subscription, final PluginObservationEvent.Kind kind,
                            final PluginObservationEvent.Outcome outcome, final long elapsed, final PluginObservationEvent.Matching matching) {
            if (records.get() >= eventLimit) {
                budgetDrops.increment();
                return;
            }
            int count = records.incrementAndGet();
            if (count >= eventLimit) {
                budgetDrops.increment();
                if (count == eventLimit) {
                    offer(new PluginObservationEvent(request, 0, 0, "observer", "UNKNOWN", "", PluginObservationEvent.Kind.TRUNCATED,
                            PluginObservationEvent.Outcome.NONE, 0, null));
                }
                return;
            }
            offer(new PluginObservationEvent(request, span.invocation, subscription, span.identity.label(), span.identity.implementation(),
                    span.trace, kind, outcome, elapsed, matching));
        }
    }

    /** Construction and each returned-publisher subscription have separate identities/durations. */
    public final class Span implements PluginObservationContext.Invocation {

        private final Scope scope;

        private final long invocation;

        private final Identity identity;

        private final String trace;

        private final long constructionStarted = System.nanoTime();

        private Span(final Scope scope, final long invocation, final Identity identity, final String trace) {
            this.scope = scope;
            this.invocation = invocation;
            this.identity = identity;
            this.trace = trace;
        }

        /**
         * Record construction/skip, without callbacks into request code.
         * @param kind actual boundary
         */
        public void constructed(final PluginObservationEvent.Kind kind) {
            scope.record(this, 0, kind, kind == PluginObservationEvent.Kind.CONSTRUCTION_ERROR || kind == PluginObservationEvent.Kind.SKIP_ERROR
                    ? PluginObservationEvent.Outcome.ERROR : PluginObservationEvent.Outcome.NONE, System.nanoTime() - constructionStarted, null);
        }

        /**
         * Observe the same returned publisher, without replay, caching or detached subscriptions.
         * @param publisher original returned publisher
         * @return observed publisher
         */
        public Mono<Void> publisher(final Mono<Void> publisher) {
            if (Objects.isNull(publisher)) {
                return null;
            }
            return Mono.defer(() -> {
                long subscription = scope.sequence.incrementAndGet();
                long started = System.nanoTime();
                scope.record(this, subscription, PluginObservationEvent.Kind.SUBSCRIBED, PluginObservationEvent.Outcome.NONE, 0, null);
                return publisher.doFinally(signal -> scope.record(this, subscription, PluginObservationEvent.Kind.TERMINAL,
                        outcome(signal), System.nanoTime() - started, null));
            });
        }

        @Override
        public boolean acceptsMatching() {
            return scope.records.get() < eventLimit - 1 && !closed.get();
        }

        @Override
        public void matching(final PluginObservationEvent.Matching matching) {
            scope.record(this, 0, PluginObservationEvent.Kind.MATCHING, PluginObservationEvent.Outcome.NONE, 0, matching);
        }
    }

    private record Identity(String label, String implementation) {
    }

    /**
     * Export diagnostics; counts are sampled and dropped records are explicit.
     * @param enabled instrumentation configuration
     * @param queued bounded queue occupancy
     * @param queueDrops saturated/closed delivery drops
     * @param budgetDrops per-root budget drops
     * @param consumerFailures exporter exceptions
     */
    public record Stats(boolean enabled, int queued, long queueDrops, long budgetDrops, long consumerFailures) {
    }
}
