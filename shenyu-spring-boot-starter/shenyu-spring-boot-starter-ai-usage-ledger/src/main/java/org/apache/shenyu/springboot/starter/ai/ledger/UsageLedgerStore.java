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

import java.time.Instant;
import java.util.List;

/** Bounded queryable sink; applications may replace it with a durable tenant-authorized backend. */
public interface UsageLedgerStore {

    /**
     * Atomically deduplicate and append without rewriting an original estimate.
     * @param row immutable event
     * @return explicit storage outcome
     */
    AppendResult append(LedgerRecord row);

    /**
     * Read only the scope authorized by the application.
     * @param scope trusted authorization, never a tenant from query parameters
     * @param limit bounded row limit, maximum 1000
     * @return immutable ordered page
     */
    List<LedgerRecord> query(Scope scope, int limit);

    /**
     * Lookup one row within the same authorized scope.
     * @param scope trusted authority
     * @param id record identity
     * @return matching row or null
     */
    LedgerRecord find(Scope scope, String id);

    /**
     * Remove a scope's retained rows, including idempotency state.
     * @param scope explicit delete authority supplied by the application
     * @return number of removed rows
     */
    int delete(Scope scope);

    /**
     * Store observations without tenant identities as labels.
     * @return bounded global runtime counters for operators
     */
    Diagnostics diagnostics();

    /** Atomic append outcome. */
    enum AppendResult {
        APPENDED, DUPLICATE, CONFLICT, FULL
    }

    /**
     * Server-authorized namespace/tenant and optional application constraint.
     * @param namespace authoritative namespace
     * @param tenant authenticated tenant
     * @param application application constraint, null means explicitly all apps in this tenant
     */
    record Scope(String namespace, String tenant, String application) {
        public Scope {
            LedgerRecord.required(namespace);
            LedgerRecord.required(tenant);
            LedgerRecord.optional(application);
        }
    }

    /**
     * Metadata-only store diagnostics.
     * @param retained retained rows
     * @param capacity hard maximum
     * @param accepted accepted records
     * @param duplicates redeliveries
     * @param conflicts identity collisions with different evidence
     * @param rejected capacity drops
     * @param expired expired records
     * @param observedAt operator observation time
     * @param epoch opaque store lifetime; a reset is observable, not an exact crash-loss count
     */
    record Diagnostics(int retained, int capacity, long accepted, long duplicates, long conflicts, long rejected, long expired, Instant observedAt, String epoch) {
    }
}
