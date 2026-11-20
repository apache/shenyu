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

package org.apache.shenyu.springboot.starter.ai.ledger;

import org.apache.shenyu.common.ledger.LedgerRecord;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.LongSupplier;

/** Finite volatile reference sink. Rejects new rows at capacity rather than evicting live dedup keys. */
public final class InMemoryUsageLedger implements UsageLedgerStore {

    private final int capacity;

    private final long retentionNanos;

    private final LongSupplier time;

    private final Map<String, Retained> rows = new LinkedHashMap<>();

    private final String epoch = UUID.randomUUID().toString();

    private long accepted;

    private long duplicates;

    private long conflicts;

    private long rejected;

    private long expired;

    public InMemoryUsageLedger(final int capacity, final Duration retention) {
        this(capacity, retention, System::nanoTime);
    }

    InMemoryUsageLedger(final int capacity, final Duration retention, final LongSupplier time) {
        if (capacity < 1 || capacity > 32768 || retention.compareTo(Duration.ofMinutes(1)) < 0 || retention.compareTo(Duration.ofDays(7)) > 0) {
            throw new IllegalArgumentException("Invalid ledger capacity or retention");
        }
        this.capacity = capacity;
        retentionNanos = retention.toNanos();
        this.time = Objects.requireNonNull(time, "time");
    }

    @Override
    public synchronized AppendResult append(final LedgerRecord row) {
        Objects.requireNonNull(row, "row");
        expire();
        Retained previous = rows.get(row.id());
        if (Objects.nonNull(previous)) {
            if (previous.row().sameEvidence(row)) {
                duplicates++;
                return AppendResult.DUPLICATE;
            }
            conflicts++;
            return AppendResult.CONFLICT;
        }
        if (rows.size() >= capacity) {
            rejected++;
            return AppendResult.FULL;
        }
        rows.put(row.id(), new Retained(row, time.getAsLong()));
        accepted++;
        return AppendResult.APPENDED;
    }

    @Override
    public synchronized List<LedgerRecord> query(final Scope scope, final int limit) {
        Objects.requireNonNull(scope, "scope");
        if (limit < 1 || limit > MAX_QUERY_LIMIT) {
            throw new IllegalArgumentException("Ledger query limit must be between 1 and " + MAX_QUERY_LIMIT);
        }
        expire();
        List<LedgerRecord> result = new ArrayList<>();
        for (Retained retained : rows.values()) {
            if (allowed(scope, retained.row())) {
                result.add(retained.row());
                if (result.size() == limit) {
                    break;
                }
            }
        }
        return List.copyOf(result);
    }

    @Override
    public synchronized LedgerRecord find(final Scope scope, final String id) {
        Objects.requireNonNull(scope, "scope");
        expire();
        Retained found = rows.get(id);
        return Objects.nonNull(found) && allowed(scope, found.row()) ? found.row() : null;
    }

    @Override
    public synchronized int delete(final Scope scope) {
        Objects.requireNonNull(scope, "scope");
        expire();
        int before = rows.size();
        rows.values().removeIf(retained -> allowed(scope, retained.row()));
        return before - rows.size();
    }

    @Override
    public synchronized Diagnostics diagnostics() {
        expire();
        return new Diagnostics(rows.size(), capacity, accepted, duplicates, conflicts, rejected, expired, Instant.now(), epoch);
    }

    private void expire() {
        long now = time.getAsLong();
        Iterator<Retained> iterator = rows.values().iterator();
        while (iterator.hasNext()) {
            if (now - iterator.next().insertedAt() >= retentionNanos) {
                iterator.remove();
                expired++;
            } else {
                break;
            }
        }
    }

    private boolean allowed(final Scope scope, final LedgerRecord row) {
        return scope.namespace().equals(row.facts().namespace()) && scope.tenant().equals(row.facts().tenant())
                && (Objects.isNull(scope.application()) || scope.application().equals(row.facts().application()));
    }

    private record Retained(LedgerRecord row, long insertedAt) {
    }
}
