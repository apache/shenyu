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

package org.apache.shenyu.springboot.starter.agent.audit;

import org.apache.shenyu.common.audit.AgentInvocationAuditEvent;
import org.apache.shenyu.common.audit.AgentInvocationAuditObserver;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.collector.AbstractLogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.LongAdder;

/**
 * Metadata adapter using ShenYu's existing bounded logging collector and queue instrumentation.
 */
public final class AgentInvocationAuditCollector extends AbstractLogCollector<AgentInvocationAuditCollector.Client,
        AgentInvocationAuditCollector.AuditLog, GenericGlobalConfig> implements AgentInvocationAuditObserver {

    private final GenericGlobalConfig config = new GenericGlobalConfig();

    private final Client client;

    public AgentInvocationAuditCollector(final int capacity, final AgentInvocationAuditConsumer consumer) {
        if (capacity < 1 || capacity > 65536) {
            throw new IllegalArgumentException("Audit queue capacity must be between 1 and 65536");
        }
        config.setBufferQueueSize(capacity);
        client = new Client(Objects.requireNonNull(consumer, "consumer"));
        client.initClient(config);
    }

    @Override
    public void record(final AgentInvocationAuditEvent event) {
        if (Objects.nonNull(event)) {
            collect(new AuditLog(event));
        }
    }

    public long getFailedRecords() {
        return client.failedRecords.sum();
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
    protected void desensitizeLog(final AuditLog log, final KeyWordMatch match, final String algorithm) {
        // Only the immutable, credential-free audit contract is populated; never copy an exchange.
    }

    public static final class AuditLog extends ShenyuRequestLog {

        private final AgentInvocationAuditEvent event;

        private AuditLog(final AgentInvocationAuditEvent event) {
            this.event = event;
            setSelectorId(event.getSelectorId().orElse(null));
        }
    }

    public static final class Client extends AbstractLogConsumeClient<GenericGlobalConfig, AuditLog> {

        private final AgentInvocationAuditConsumer consumer;

        private final LongAdder failedRecords = new LongAdder();

        private Client(final AgentInvocationAuditConsumer consumer) {
            this.consumer = consumer;
        }

        @Override
        public boolean initClient0(final GenericGlobalConfig config) {
            return true;
        }

        @Override
        public void consume0(final List<AuditLog> logs) {
            try {
                consumer.accept(logs.stream().map(log -> log.event).toList());
            } catch (RuntimeException error) {
                failedRecords.add(logs.size());
                // Do not log sink exception messages: they may contain backend credentials or payloads.
                LoggerFactory.getLogger(AgentInvocationAuditCollector.class).warn("Agent invocation audit sink failed");
            }
        }

        @Override
        public void close0() {
            // The application owns the sink lifecycle.
        }
    }
}
