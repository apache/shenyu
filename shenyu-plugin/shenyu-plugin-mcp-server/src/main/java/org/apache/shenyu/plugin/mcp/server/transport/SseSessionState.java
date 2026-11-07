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

import reactor.core.Disposable;
import reactor.core.scheduler.Scheduler;

import java.util.Objects;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

/**
 * Per-session admission state for one server-produced SSE session.
 *
 * <p>Every JSON-RPC message sent through the session reserves one event slot and the
 * UTF-8 length of its serialized payload before the event is handed to the reactive
 * stream. The reservation is released when the stream consumer actually pulls the event
 * out of the transport's buffer, which is the only unbounded segment: downstream of that
 * point Reactor Netty bounds in-flight writes with its own socket-writability water
 * marks. When the budget is exhausted the send is rejected; when pending output makes no
 * consumption progress for the policy's stall timeout the whole session is terminated
 * through an idempotent terminal path.</p>
 *
 * <p>Releases happen in FIFO event order because the consumer pulls events in emission
 * order and reservations are queued in the same order, so a single poll per released
 * event keeps the byte accounting exact without tagging individual events.</p>
 */
final class SseSessionState {

    /**
     * Outcome of one admission attempt.
     */
    enum Reservation {

        /**
         * The event was accepted and its budget reserved.
         */
        ACCEPTED,

        /**
         * The single event exceeds the whole pending-byte budget; the session stays
         * usable for other messages.
         */
        OVERSIZED,

        /**
         * The event does not fit the remaining budget because the consumer is too slow;
         * the session is terminated.
         */
        OVERFLOW,

        /**
         * The session is already terminated; nothing is accepted anymore.
         */
        TERMINATED
    }

    /**
     * Bounded reasons why a session reached its terminal state. Used for counters and
     * log lines only; never carries session identifiers or message content.
     */
    enum TerminalReason {

        /**
         * The SSE consumer disconnected (cancel or dispose of the event stream).
         */
        CLIENT_DISCONNECTED,

        /**
         * Pending output made no consumption progress within the stall timeout.
         */
        OUTPUT_STALLED,

        /**
         * A send was rejected because the pending budget was exhausted.
         */
        OUTPUT_OVERFLOW,

        /**
         * The transport provider is shutting down.
         */
        PROVIDER_CLOSED
    }

    private final SseSessionPolicy policy;

    private final Scheduler stallScheduler;

    private final AtomicReference<Disposable> stallTask;

    private final Queue<Long> reservedLengths = new ConcurrentLinkedQueue<>();

    private final AtomicInteger pendingEvents = new AtomicInteger();

    private final AtomicLong pendingBytes = new AtomicLong();

    private final AtomicBoolean terminated = new AtomicBoolean();

    private final AtomicLong lastProgressAt = new AtomicLong(System.nanoTime());

    private final Consumer<TerminalReason> terminalAction;

    SseSessionState(final SseSessionPolicy policy, final Scheduler stallScheduler,
                    final Consumer<TerminalReason> terminalAction) {
        this.policy = Objects.requireNonNull(policy, "policy must not be null");
        this.stallScheduler = Objects.requireNonNull(stallScheduler, "stallScheduler must not be null");
        this.terminalAction = Objects.requireNonNull(terminalAction, "terminalAction must not be null");
        this.stallTask = new AtomicReference<>();
    }

    /**
     * Reserve one event slot and the serialized byte length of the payload.
     *
     * @param byteLength the UTF-8 length of the serialized payload
     * @return the admission outcome; only {@link Reservation#ACCEPTED} allows the caller
     *         to emit the event
     */
    Reservation tryReserve(final int byteLength) {
        synchronized (this) {
            if (terminated.get()) {
                return Reservation.TERMINATED;
            }
            if (byteLength > policy.getMaxPendingBytes()) {
                return Reservation.OVERSIZED;
            }
            if (pendingEvents.get() + 1 > policy.getMaxPendingEvents()
                    || pendingBytes.get() + byteLength > policy.getMaxPendingBytes()) {
                return Reservation.OVERFLOW;
            }
            pendingEvents.incrementAndGet();
            pendingBytes.addAndGet(byteLength);
            reservedLengths.add((long) byteLength);
            if (pendingEvents.get() == 1) {
                scheduleStallCheck();
            }
            return Reservation.ACCEPTED;
        }
    }

    /**
     * Release the reservation of the next accepted event, called when the SSE consumer
     * pulls one accepted event out of the transport buffer. An event emitted without a
     * reservation (the initial endpoint event) finds an empty queue and is ignored.
     * Guarded by the same monitor as {@link #tryReserve(int)} and
     * {@link #terminate(TerminalReason)} so a concurrent terminal cleanup can never
     * interleave with a release and drive the pending counters negative.
     */
    void release() {
        synchronized (this) {
            Long length = reservedLengths.poll();
            if (Objects.isNull(length)) {
                return;
            }
            pendingEvents.decrementAndGet();
            pendingBytes.addAndGet(-length);
            lastProgressAt.set(System.nanoTime());
            if (pendingEvents.get() <= 0) {
                Disposable task = stallTask.getAndSet(null);
                if (Objects.nonNull(task)) {
                    task.dispose();
                }
            }
        }
    }

    /**
     * Terminate the session exactly once, whichever caller wins the transition. The
     * terminal action (state cleanup, session close, stream completion) runs on the
     * winning thread only. Guarded by the same monitor as reservation and release so
     * the counter reset cannot interleave with either; the terminal action may call
     * back into {@link #release()} through a synchronous drain, which reenters the
     * same monitor on the same thread.
     *
     * @param reason why the session is terminated
     * @return true when this call performed the terminal transition
     */
    boolean terminate(final TerminalReason reason) {
        synchronized (this) {
            if (!terminated.compareAndSet(false, true)) {
                return false;
            }
            Disposable task = stallTask.getAndSet(null);
            if (Objects.nonNull(task)) {
                task.dispose();
            }
            reservedLengths.clear();
            pendingEvents.set(0);
            pendingBytes.set(0);
            terminalAction.accept(reason);
            return true;
        }
    }

    /**
     * Whether the session already reached its terminal state.
     *
     * @return true when terminated
     */
    boolean isTerminated() {
        return terminated.get();
    }

    /**
     * Current number of accepted but unreleased events.
     *
     * @return the pending-event count
     */
    int getPendingEvents() {
        return pendingEvents.get();
    }

    /**
     * Current number of accepted but unreleased serialized bytes.
     *
     * @return the pending-byte count
     */
    long getPendingBytes() {
        return pendingBytes.get();
    }

    private void scheduleStallCheck() {
        lastProgressAt.set(System.nanoTime());
        Disposable task = stallScheduler.schedule(this::checkStall,
                policy.getStallTimeout().toMillis(), TimeUnit.MILLISECONDS);
        stallTask.set(task);
    }

    private void checkStall() {
        if (terminated.get() || pendingEvents.get() <= 0) {
            return;
        }
        long elapsedNanos = System.nanoTime() - lastProgressAt.get();
        long timeoutNanos = policy.getStallTimeout().toNanos();
        if (elapsedNanos >= timeoutNanos) {
            terminate(TerminalReason.OUTPUT_STALLED);
        } else {
            Disposable task = stallScheduler.schedule(this::checkStall,
                    TimeUnit.NANOSECONDS.toMillis(timeoutNanos - elapsedNanos),
                    TimeUnit.MILLISECONDS);
            stallTask.set(task);
        }
    }
}
