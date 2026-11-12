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

package org.apache.shenyu.plugin.ai.proxy.enhanced.evaluation;

import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.plugin.ai.common.evaluation.AiEvaluationExportRecord;
import org.apache.shenyu.plugin.ai.common.evaluation.AiEvaluationSink;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.LongAdder;
import java.util.concurrent.locks.LockSupport;

/**
 * The bounded asynchronous exporter of the AI evaluation pipeline (issue #8256).
 *
 * <p>{@link #offer} is the only entry the request path ever touches: a
 * non-blocking bounded-queue admission that counts a drop when the queue is full
 * and never blocks, so sampling can never delay or alter a business response. A
 * dedicated daemon thread drains the queue in batches into the sink; a failing
 * sink is the sink's own concern, an export batch is dropped, not replayed, and
 * the overflow is always visible in the accepted/dropped counters. Retained state
 * is bounded by the queue capacity — no exchange or request object is ever held,
 * only immutable versioned records.</p>
 */
public final class AiEvaluationExporter implements AutoCloseable {

    private final LinkedBlockingQueue<AiEvaluationExportRecord> queue;

    private final int batchSize;

    private final AiEvaluationSink sink;

    private final AtomicBoolean running = new AtomicBoolean(true);

    private final LongAdder accepted = new LongAdder();

    private final LongAdder dropped = new LongAdder();

    private final Thread worker;

    public AiEvaluationExporter(final int queueCapacity, final int batchSize, final AiEvaluationSink sink) {
        this(queueCapacity, batchSize, sink, true);
    }

    AiEvaluationExporter(final int queueCapacity, final int batchSize, final AiEvaluationSink sink,
                         final boolean autoStart) {
        this.queue = new LinkedBlockingQueue<>(queueCapacity);
        this.batchSize = Math.max(1, batchSize);
        this.sink = sink;
        this.worker = ShenyuThreadFactory.create("shenyu-ai-evaluation-exporter", true).newThread(this::drainLoop);
        if (autoStart) {
            this.worker.start();
        }
    }

    /**
     * Non-blocking bounded admission of one versioned record.
     *
     * @param record the versioned export record
     */
    public void offer(final AiEvaluationExportRecord record) {
        if (queue.offer(record)) {
            accepted.increment();
        } else {
            dropped.increment();
        }
    }

    /**
     * Drain at most one batch into the sink; package-private so tests can drive
     * the pipeline deterministically without waiting on the worker thread.
     *
     * @return the number of exported records
     */
    int drainOnce() {
        List<AiEvaluationExportRecord> batch = new ArrayList<>(batchSize);
        queue.drainTo(batch, batchSize);
        if (!batch.isEmpty()) {
            try {
                sink.export(batch);
            } catch (RuntimeException ignored) {
                // a failing sink drops its batch instead of killing the drain thread
            }
        }
        return batch.size();
    }

    long getAccepted() {
        return accepted.sum();
    }

    long getDropped() {
        return dropped.sum();
    }

    boolean queueIsEmpty() {
        return queue.isEmpty();
    }

    private void drainLoop() {
        while (running.get()) {
            if (drainOnce() == 0) {
                LockSupport.parkNanos(50_000_000L);
            }
        }
    }

    @Override
    public void close() {
        running.set(false);
        worker.interrupt();
        while (!queue.isEmpty()) {
            drainOnce();
        }
        sink.close();
    }
}
