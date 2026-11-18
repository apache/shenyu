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
import org.apache.shenyu.common.metrics.LoggingQueueMetrics;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentInvocationAuditCollectorTest {

    @Test
    void defaultLoggingSinkWritesMetadataOnly() throws Exception {
        ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger) org.slf4j.LoggerFactory.getLogger("org.apache.shenyu.agent.invocation.audit");
        List<String> messages = new CopyOnWriteArrayList<>();
        ch.qos.logback.core.AppenderBase<ch.qos.logback.classic.spi.ILoggingEvent> appender = new ch.qos.logback.core.AppenderBase<>() {
            @Override
            protected void append(final ch.qos.logback.classic.spi.ILoggingEvent event) {
                messages.add(event.getFormattedMessage());
            }
        };
        appender.start();
        logger.addAppender(appender);
        try (AgentInvocationAuditCollector collector = new AgentInvocationAuditCollector(8, AgentInvocationAuditConsumer.LOGGING)) {
            collector.start();
            collector.record(AgentInvocationAuditEvent.logical().requestId("audit-request").apiKeyId("internal-id").outcome("SUCCESS").build());
            await().atMost(Duration.ofSeconds(5)).until(() -> !messages.isEmpty());
            assertTrue(messages.get(0).contains("request=audit-request"));
            assertTrue(messages.get(0).contains("apiKeyId=internal-id"));
            assertTrue(messages.get(0).contains("schema=1"));
            assertTrue(messages.stream().noneMatch(message -> message.contains("Authorization") || message.contains("requestBody")));
        } finally {
            logger.detachAppender(appender);
            appender.stop();
        }
    }

    @Test
    void deliversOffRequestThreadThroughExistingCollector() throws Exception {
        List<AgentInvocationAuditEvent> delivered = new CopyOnWriteArrayList<>();
        AtomicReference<Thread> worker = new AtomicReference<>();
        try (AgentInvocationAuditCollector collector = new AgentInvocationAuditCollector(8, batch -> {
            worker.set(Thread.currentThread());
            delivered.addAll(batch);
        })) {
            collector.start();
            collector.record(event("one"));
            await().atMost(Duration.ofSeconds(5)).until(() -> delivered.size() == 1);
            assertNotEquals(Thread.currentThread(), worker.get());
            assertEquals(1, metrics().getAccepted());
            assertEquals(8, metrics().getCapacity());
            assertEquals(0, metrics().getDropped());
        }
        assertTrue(LoggingQueueMetrics.registered().stream().noneMatch(metric -> metric.getCollector().equals(AgentInvocationAuditCollector.class.getName())));
    }

    @Test
    void slowSinkDoesNotBlockAdmissionAndOverflowUsesExistingMetrics() throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        try (AgentInvocationAuditCollector collector = new AgentInvocationAuditCollector(2, batch -> {
            entered.countDown();
            try {
                release.await(5, TimeUnit.SECONDS);
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
            }
        })) {
            collector.start();
            collector.record(event("in-flight"));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            try {
                collector.record(event("queued-one"));
                collector.record(event("queued-two"));
                collector.record(event("overflow"));
                assertEquals(3, metrics().getAccepted());
                assertEquals(1, metrics().getDropped());
                assertEquals(2, metrics().getQueuedRecords());
            } finally {
                release.countDown();
            }
        }
    }

    @Test
    void countsSinkFailuresAndCloseStopsAdmission() throws Exception {
        AgentInvocationAuditCollector collector = new AgentInvocationAuditCollector(4, batch -> {
            throw new IllegalStateException("secret must not be logged");
        });
        try (collector) {
            collector.start();
            collector.record(event("failed"));
            await().atMost(Duration.ofSeconds(5)).until(() -> collector.getFailedRecords() == 1);
        }
        collector.record(event("after-close"));
        assertEquals(1, collector.getFailedRecords());
    }

    @Test
    void rejectsUnboundedCapacity() {
        assertThrows(IllegalArgumentException.class, () -> new AgentInvocationAuditCollector(0, batch -> { }));
        assertThrows(IllegalArgumentException.class, () -> new AgentInvocationAuditCollector(65537, batch -> { }));
    }

    private LoggingQueueMetrics metrics() {
        return LoggingQueueMetrics.registered().stream()
                .filter(metric -> metric.getCollector().equals(AgentInvocationAuditCollector.class.getName())
                        && metric.getMode() == LoggingQueueMetrics.Mode.SINGLE).findFirst().orElseThrow();
    }

    private AgentInvocationAuditEvent event(final String id) {
        return AgentInvocationAuditEvent.logical().requestId(id).outcome("SUCCESS").build();
    }
}
