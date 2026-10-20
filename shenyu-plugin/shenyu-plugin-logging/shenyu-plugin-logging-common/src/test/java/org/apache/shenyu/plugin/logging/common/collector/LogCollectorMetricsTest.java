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

package org.apache.shenyu.plugin.logging.common.collector;

import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.metrics.LoggingQueueMetrics;
import org.apache.shenyu.common.metrics.LoggingQueueMetrics.Mode;
import org.apache.shenyu.plugin.logging.common.AbstractLoggingPlugin;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.common.sampler.Sampler;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedConstruction;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.BlockingQueue;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.verify;

/**
 * Verify admission accounting against real queues with the consumer held idle.
 */
class LogCollectorMetricsTest {

    private TestCollector collector;

    private MockedConstruction<ShenyuThreadPoolExecutor> executors;

    @BeforeEach
    void setUp() {
        executors = mockConstruction(ShenyuThreadPoolExecutor.class);
        collector = new TestCollector();
        collector.config.setBufferQueueSize(1);
    }

    @AfterEach
    void tearDown() throws Exception {
        try {
            reset(collector.client);
            collector.close();
        } finally {
            executors.close();
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void countsOnlyQueueAdmissionRejections(final boolean multi) throws Exception {
        collector.multi = multi;
        collector.collect(log("selector"));
        collector.start();
        final LoggingQueueMetrics metrics = metrics(multi);
        assertEquals(0, metrics.getDropped());
        collector.collect(log("selector"));
        collector.collect(log("selector"));
        assertEquals(1, metrics.getAccepted());
        assertEquals(1, metrics.getDropped());
        assertEquals(1, metrics.getQueuedRecords());
        assertEquals(1, metrics.getCapacity());
        collector.collect(null);
        collector.clientAvailable = false;
        collector.collect(log("selector"));
        assertEquals(1, metrics.getAccepted());
        assertEquals(1, metrics.getDropped());
        collector.clientAvailable = true;
        collector.close();
        collector.collect(log("selector"));
        assertEquals(1, metrics.getDropped());
        assertEquals(0, metrics.getQueuedRecords());
        assertEquals(0, metrics.getCapacity());
        assertFalse(LoggingQueueMetrics.registered().contains(metrics));
        verify(collector.client).consume(anyList());
    }

    @Test
    void reflectsLiveSingleClientCapacityWithoutDiscardingBacklog() {
        collector.start();
        final LoggingQueueMetrics metrics = metrics(false);
        collector.collect(log("first"));
        collector.config.setBufferQueueSize(3);
        collector.start();
        collector.collect(log("second"));
        collector.collect(log("third"));
        assertEquals(3, metrics.getCapacity());
        assertEquals(3, metrics.getQueuedRecords());
        collector.config.setBufferQueueSize(1);
        collector.start();
        collector.collect(log("fourth"));
        assertEquals(3, metrics.getAccepted());
        assertEquals(1, metrics.getDropped());
        assertEquals(1, metrics.getCapacity());
        assertEquals(3, metrics.getQueuedRecords());
        BlockingQueue<?> queue = (BlockingQueue<?>) ReflectionTestUtils.getField(collector, "bufferQueue");
        queue.clear();
        assertEquals(0, metrics.getQueuedRecords());
        collector.collect(log("fifth"));
        assertEquals(4, metrics.getAccepted());
        assertEquals(1, metrics.getQueuedRecords());
    }

    @Test
    void aggregatesSelectorQueuesUsingTheirActualCapacities() {
        collector.multi = true;
        collector.start();
        final LoggingQueueMetrics metrics = metrics(true);
        assertEquals(0, metrics.getCapacity());
        collector.collect(log("first"));
        collector.config.setBufferQueueSize(2);
        collector.start();
        collector.collect(log("second"));
        collector.collect(log("second"));
        collector.collect(log("first"));
        collector.collect(log("second"));
        assertEquals(3, metrics.getAccepted());
        assertEquals(2, metrics.getDropped());
        assertEquals(3, metrics.getQueuedRecords());
        assertEquals(3, metrics.getCapacity());
        assertEquals(0, metrics(false).getCapacity());
        collector.processMultiClientBufferQueues(1, 100);
        assertEquals(1, metrics.getQueuedRecords());
        assertEquals(3, metrics.getCapacity());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void accountsForConcurrentProducers(final boolean multi) {
        collector.multi = multi;
        collector.config.setBufferQueueSize(10);
        collector.start();
        IntStream.range(0, 1000).parallel().forEach(index -> collector.collect(log("selector")));
        final LoggingQueueMetrics metrics = metrics(multi);
        assertEquals(10, metrics.getAccepted());
        assertEquals(990, metrics.getDropped());
        assertEquals(10, metrics.getQueuedRecords());
        assertEquals(10, metrics.getCapacity());
    }

    @Test
    void backendFailuresAreNotAdmissionDrops() throws Exception {
        collector.multi = true;
        collector.start();
        collector.collect(log("selector"));
        doThrow(new Exception("backend unavailable")).when(collector.client).consume(anyList());
        collector.processMultiClientBufferQueues(1, 100);
        final LoggingQueueMetrics metrics = metrics(true);
        assertEquals(1, metrics.getAccepted());
        assertEquals(0, metrics.getDropped());
        assertEquals(0, metrics.getQueuedRecords());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void consumingUpdatesOccupancyWithoutChangingAdmissionCounters(final boolean multi) {
        collector.multi = multi;
        collector.config.setBufferQueueSize(2);
        collector.start();
        collector.collect(log("selector"));
        collector.collect(log("selector"));
        final LoggingQueueMetrics metrics = metrics(multi);
        if (multi) {
            collector.processMultiClientBufferQueues(1, 100);
        } else {
            BlockingQueue<?> queue = (BlockingQueue<?>) ReflectionTestUtils.getField(collector, "bufferQueue");
            ReflectionTestUtils.invokeMethod(collector, "processBufferQueue", queue, 1, 100, new ArrayList<>(), 0L);
        }
        assertEquals(1, metrics.getQueuedRecords());
        assertEquals(2, metrics.getCapacity());
        assertEquals(2, metrics.getAccepted());
        assertEquals(0, metrics.getDropped());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void samplingDoesNotCountAsQueueOverflow(final boolean multi) {
        collector.multi = multi;
        collector.start();
        collector.collect(log("sampled-out-selector"));
        GenericApiConfig apiConfig = new GenericApiConfig();
        apiConfig.setSampler(Sampler.NEVER_SAMPLE);
        SelectorData selector = new SelectorData();
        selector.setId("sampled-out-selector");
        RuleData rule = new RuleData();
        rule.setId("sampled-out-rule");
        rule.setSelectorId(selector.getId());
        AbstractLogPluginDataHandler.getSelectApiConfigMap().put(selector.getId(), apiConfig);
        try {
            AtomicBoolean chainExecuted = new AtomicBoolean();
            loggingPlugin().doExecute(MockServerWebExchange.from(MockServerHttpRequest.get("/sampled-out").build()), exchange -> {
                chainExecuted.set(true);
                return Mono.empty();
            }, selector, rule).block();
            assertTrue(chainExecuted.get());
            assertEquals(1, metrics(multi).getQueuedRecords());
            assertEquals(1, metrics(multi).getAccepted());
            assertEquals(0, metrics(multi).getDropped());
        } finally {
            AbstractLogPluginDataHandler.getSelectApiConfigMap().remove(selector.getId());
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void modeChangesDoNotHideRetainedBacklogs(final boolean multi) {
        collector.multi = multi;
        collector.start();
        collector.collect(log("selector"));
        final LoggingQueueMetrics metrics = metrics(multi);
        try {
            collector.multi = !multi;
            assertEquals(1, metrics.getQueuedRecords());
            assertEquals(0, metrics.getCapacity());
            assertEquals(1, metrics.getAccepted());
            assertEquals(0, metrics.getDropped());
        } finally {
            collector.multi = multi;
        }
        assertEquals(1, metrics.getCapacity());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void retainsCountersAndRestoresGaugesOnRestart(final boolean multi) throws Exception {
        collector.multi = multi;
        collector.start();
        collector.collect(log("selector"));
        collector.collect(log("selector"));
        final LoggingQueueMetrics metrics = metrics(multi);
        collector.close();
        collector.start();
        assertSame(metrics, metrics(multi));
        assertEquals(1, metrics.getDropped());
        assertEquals(0, metrics.getQueuedRecords());
        assertEquals(1, metrics.getCapacity());
        collector.collect(log("selector"));
        assertEquals(2, metrics.getAccepted());
    }

    private LoggingQueueMetrics metrics(final boolean multi) {
        return LoggingQueueMetrics.registered().stream()
                .filter(metric -> metric.getCollector().equals(TestCollector.class.getName()))
                .filter(metric -> metric.getMode() == (multi ? Mode.MULTI : Mode.SINGLE))
                .findFirst().orElseThrow(() -> new AssertionError("Collector did not register queue metrics"));
    }

    private AbstractLoggingPlugin<ShenyuRequestLog> loggingPlugin() {
        return new AbstractLoggingPlugin<>() {
            @Override
            protected LogCollector<ShenyuRequestLog> logCollector() {
                return collector;
            }

            @Override
            protected PluginEnum pluginEnum() {
                return PluginEnum.LOGGING_KAFKA;
            }

            @Override
            protected ShenyuRequestLog doLogExecute(final ServerWebExchange exchange, final SelectorData selector, final RuleData rule) {
                throw new AssertionError("A sampled-out request must not produce a log");
            }
        };
    }

    private ShenyuRequestLog log(final String selector) {
        ShenyuRequestLog log = new ShenyuRequestLog();
        log.setSelectorId(selector);
        return log;
    }

    private static final class TestCollector extends AbstractLogCollector<AbstractLogConsumeClient<?, ShenyuRequestLog>, ShenyuRequestLog, GenericGlobalConfig> {

        private final GenericGlobalConfig config = new GenericGlobalConfig();

        private final AbstractLogConsumeClient<?, ShenyuRequestLog> client = mock(AbstractLogConsumeClient.class);

        private boolean multi;

        private boolean clientAvailable = true;

        @Override
        protected AbstractLogConsumeClient<?, ShenyuRequestLog> getLogConsumeClient() {
            return clientAvailable ? client : null;
        }

        @Override
        protected GenericGlobalConfig getLogCollectConfig() {
            return config;
        }

        @Override
        protected boolean getMultiClient() {
            return multi;
        }

        @Override
        protected void desensitizeLog(final ShenyuRequestLog log, final KeyWordMatch match, final String algorithm) {
        }
    }
}
