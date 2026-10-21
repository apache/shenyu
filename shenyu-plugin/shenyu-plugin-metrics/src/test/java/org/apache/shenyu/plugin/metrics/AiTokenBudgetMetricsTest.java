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

package org.apache.shenyu.plugin.metrics;

import io.prometheus.client.CollectorRegistry;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.metrics.AiTokenBudgetObserver;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import org.apache.shenyu.plugin.metrics.prometheus.PrometheusMetricsRegister;
import org.apache.shenyu.plugin.metrics.reporter.MetricsReporter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Real Prometheus registry coverage for bounded AI budget events and distinct token counters.
 */
class AiTokenBudgetMetricsTest {

    @BeforeEach
    void registerMetrics() {
        MetricsReporter.clean();
        CollectorRegistry.defaultRegistry.clear();
        MetricsReporter.register(new PrometheusMetricsRegister());
    }

    @AfterEach
    void cleanMetrics() {
        MetricsReporter.clean();
        CollectorRegistry.defaultRegistry.clear();
    }

    @Test
    void testEventsAndTokenAmountsAreExposedSeparatelyWithoutIdentityLabels() {
        final AiTokenBudgetObserver observer = observer();
        for (AiTokenBudgetObserver.Event event : AiTokenBudgetObserver.Event.values()) {
            observer.record(event, 10L);
            assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AI_TOKEN_BUDGET_EVENTS_TOTAL,
                    new String[]{"event"}, new String[]{event.name().toLowerCase(Locale.ROOT)}));
        }
        assertEquals(10.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AI_TOKEN_RESERVED_TOKENS_TOTAL));
        assertEquals(10.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AI_TOKEN_ACTUAL_TOKENS_TOTAL));
    }

    @Test
    void testReliableZeroAndExpiredWindowAreDistinctFromUnknownUsage() {
        final AiTokenBudgetObserver observer = observer();
        observer.record(AiTokenBudgetObserver.Event.ADMITTED, 20L);
        observer.record(AiTokenBudgetObserver.Event.USAGE_REPORTED, 0L);
        observer.record(AiTokenBudgetObserver.Event.WINDOW_EXPIRED, 0L);
        observer.record(AiTokenBudgetObserver.Event.USAGE_HELD, 0L);

        assertEquals(20.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AI_TOKEN_RESERVED_TOKENS_TOTAL));
        assertEquals(0.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AI_TOKEN_ACTUAL_TOKENS_TOTAL));
        assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AI_TOKEN_BUDGET_EVENTS_TOTAL,
                new String[]{"event"}, new String[]{"usage_reported"}));
    }

    @Test
    void testMetricsRunBeforeAiTokenLimiter() {
        assertTrue(PluginEnum.METRICS.getCode() < PluginEnum.AI_TOKEN_LIMITER.getCode());
    }

    private AiTokenBudgetObserver observer() {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/user/123/private-key").build());
        final ShenyuContext context = mock(ShenyuContext.class);
        when(context.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        StepVerifier.create(new MetricsPlugin().execute(exchange, ignored -> Mono.empty())).verifyComplete();
        final AiTokenBudgetObserver observer = exchange.getAttribute(Constants.METRICS_AI_TOKEN_BUDGET);
        assertNotNull(observer);
        return observer;
    }
}
