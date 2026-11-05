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

import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.collector.AbstractLogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.LongAdder;

/** Reuses the logging/audit bounded queue and its existing queue/drop instrumentation. */
public final class UsageLedgerCollector extends AbstractLogCollector<UsageLedgerCollector.Client, UsageLedgerCollector.RowLog, GenericGlobalConfig> {

    private final GenericGlobalConfig config = new GenericGlobalConfig();

    private final Client client;

    public UsageLedgerCollector(final int queueCapacity, final UsageLedgerStore store) {
        if (queueCapacity < 1 || queueCapacity > 16384) {
            throw new IllegalArgumentException("Invalid ledger queue capacity");
        }
        config.setBufferQueueSize(queueCapacity);
        client = new Client(Objects.requireNonNull(store, "store"));
        client.initClient(config);
    }

    /**
     * Queue one immutable metadata record; overflow is visible through LoggingQueueMetrics.
     * @param row credential-free evidence
     */
    public void submit(final LedgerRecord row) {
        collect(new RowLog(Objects.requireNonNull(row, "row")));
    }

    /**
     * Delivery/storage failures, without sensitive exception messages.
     * @return dropped records after sink errors or storage rejection
     */
    public long failedRecords() {
        return client.failed.sum();
    }

    @Override
    protected Client getLogConsumeClient() {
        return client;
    }

    @Override
    protected GenericGlobalConfig getLogCollectConfig() {
        return config;
    }

    @Override
    protected void desensitizeLog(final RowLog log, final KeyWordMatch match, final String algorithm) {
        // Populate only the bounded metadata contract, never ShenyuRequestLog payload/header fields.
    }

    /** Metadata envelope for the shared logging queue. */
    public static final class RowLog extends ShenyuRequestLog {

        private final LedgerRecord row;

        private RowLog(final LedgerRecord row) {
            this.row = row;
        }
    }

    /** Delivery adapter; the application owns any external store lifecycle. */
    public static final class Client extends AbstractLogConsumeClient<GenericGlobalConfig, RowLog> {

        private final UsageLedgerStore store;

        private final LongAdder failed = new LongAdder();

        private Client(final UsageLedgerStore store) {
            this.store = store;
        }

        @Override
        public boolean initClient0(final GenericGlobalConfig config) {
            return true;
        }

        @Override
        public void consume0(final List<RowLog> logs) {
            for (RowLog log : logs) {
                try {
                    UsageLedgerStore.AppendResult result = store.append(log.row);
                    if (result == UsageLedgerStore.AppendResult.FULL || result == UsageLedgerStore.AppendResult.CONFLICT) {
                        failed.increment();
                    }
                } catch (RuntimeException error) {
                    failed.increment();
                    // Never propagate/log sink message text, which may contain backend credentials.
                }
            }
        }

        @Override
        public void close0() {
            // Store ownership remains with the application.
        }
    }
}
