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
import org.apache.shenyu.common.metrics.AgentMcpCallObserver;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import org.apache.shenyu.plugin.metrics.reporter.MetricsReporter;
import org.apache.shenyu.plugin.metrics.prometheus.PrometheusMetricsRegister;
import org.apache.shenyu.plugin.metrics.spi.MetricsRegister;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.Locale;

/**
 * The Test Case For MetricsPlugin.
 */
public class MetricsPluginTest {

    private MetricsPlugin metricsPlugin;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    @BeforeEach
    public void setUp() {
        this.metricsPlugin = new MetricsPlugin();
        this.chain = Mockito.mock(ShenyuPluginChain.class);
        MockServerHttpRequest request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .build();
        ConfigurableApplicationContext context = Mockito.mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        RemoteAddressResolver remoteAddressResolver = new RemoteAddressResolver() {
        };
        Mockito.lenient().when(context.getBean(RemoteAddressResolver.class)).thenReturn(remoteAddressResolver);
        this.exchange = Mockito.spy(MockServerWebExchange.from(request));
        ShenyuContext shenyuContext = Mockito.mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
    }

    @Test
    public void testDoExecute() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        Mono<Void> result = metricsPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    @Test
    public void testGetOrder() {
        Assertions.assertEquals(metricsPlugin.getOrder(), PluginEnum.METRICS.getCode());
    }

    @Test
    public void testNamed() {
        Assertions.assertEquals(metricsPlugin.named(), PluginEnum.METRICS.getName());
    }

    @Test
    public void testRequestTypeTotalIsNotLabelledByRawPath() {
        MetricsRegister metricsRegister = Mockito.mock(MetricsRegister.class);
        MetricsReporter.register(metricsRegister);
        try {
            Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
            String rpcType = RpcTypeEnum.HTTP.getName();
            ShenyuContext shenyuContext = Mockito.mock(ShenyuContext.class);
            Mockito.lenient().when(shenyuContext.getRpcType()).thenReturn(rpcType);
            StepVerifier.create(metricsPlugin.execute(createExchange("/api/user/123", shenyuContext), chain))
                    .expectSubscription().verifyComplete();
            StepVerifier.create(metricsPlugin.execute(createExchange("/api/order/456", shenyuContext), chain))
                    .expectSubscription().verifyComplete();
            // the raw path must not be used as label value, otherwise the prometheus client keeps
            // one child series per distinct path and its children map grows without bound.
            Mockito.verify(metricsRegister, Mockito.times(2))
                    .counterIncrement(LabelNames.REQUEST_TYPE_TOTAL, new String[]{rpcType}, 1L);
        } finally {
            MetricsReporter.clean();
        }
    }

    @Test
    public void testMcpCallbackUsesOnlyBoundedOutcomes() {
        MetricsRegister metricsRegister = Mockito.mock(MetricsRegister.class);
        MetricsReporter.register(metricsRegister);
        try {
            Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
            StepVerifier.create(metricsPlugin.execute(exchange, chain)).verifyComplete();
            AgentMcpCallObserver observer = exchange.getAttribute(Constants.METRICS_AGENT_MCP_CALL);
            Assertions.assertNotNull(observer);
            for (AgentMcpCallObserver.Outcome outcome : AgentMcpCallObserver.Outcome.values()) {
                observer.record(outcome, 42);
                String[] labels = {outcome.name().toLowerCase(Locale.ROOT)};
                Mockito.verify(metricsRegister).counterIncrement(LabelNames.AGENT_MCP_CALLS_TOTAL, labels, 1L);
                Mockito.verify(metricsRegister).recordTime(LabelNames.AGENT_MCP_CALL_LATENCY, labels, 42);
            }
        } finally {
            MetricsReporter.clean();
        }
    }

    @Test
    public void testMcpTerminalMetricsAreExposedByExistingPrometheusRegistry() {
        MetricsReporter.clean();
        CollectorRegistry.defaultRegistry.clear();
        MetricsReporter.register(new PrometheusMetricsRegister());
        try {
            ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
            Mockito.when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());
            Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
            StepVerifier.create(metricsPlugin.execute(exchange, chain)).verifyComplete();
            AgentMcpCallObserver observer = exchange.getAttribute(Constants.METRICS_AGENT_MCP_CALL);
            Assertions.assertNotNull(observer);
            for (AgentMcpCallObserver.Outcome outcome : AgentMcpCallObserver.Outcome.values()) {
                observer.record(outcome, 42);
                String[] labels = {outcome.name().toLowerCase(Locale.ROOT)};
                Assertions.assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_MCP_CALLS_TOTAL, new String[]{"outcome"}, labels));
                Assertions.assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_MCP_CALL_LATENCY + "_count", new String[]{"outcome"}, labels));
                Assertions.assertEquals(42.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_MCP_CALL_LATENCY + "_sum", new String[]{"outcome"}, labels));
            }
        } finally {
            MetricsReporter.clean();
            CollectorRegistry.defaultRegistry.clear();
        }
    }

    private ServerWebExchange createExchange(final String path, final ShenyuContext shenyuContext) {
        ServerWebExchange result = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost" + path).build());
        result.getAttributes().put(Constants.CONTEXT, shenyuContext);
        return result;
    }

    @Test
    void governanceMetricsUseTheExistingPrometheusRegistryWithoutIdentityLabels() {
        MetricsReporter.clean();
        CollectorRegistry.defaultRegistry.clear();
        MetricsReporter.register(new PrometheusMetricsRegister());
        try {
            AgentRuntimeMetrics metrics = MetricsReporter.agentRuntimeMetrics();
            metrics.started(AgentRuntimeMetrics.Traffic.MCP);
            metrics.started(AgentRuntimeMetrics.Traffic.MCP);
            Assertions.assertEquals(2.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_REQUESTS_INFLIGHT,
                    new String[]{"traffic"}, new String[]{"mcp"}));
            metrics.finished(AgentRuntimeMetrics.Traffic.MCP, AgentRuntimeMetrics.Outcome.REJECTED, AgentRuntimeMetrics.Reason.AUTHENTICATION, 7);
            metrics.finished(AgentRuntimeMetrics.Traffic.MCP, AgentRuntimeMetrics.Outcome.CANCELLED, AgentRuntimeMetrics.Reason.NONE, 9);
            Assertions.assertEquals(0.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_REQUESTS_INFLIGHT,
                    new String[]{"traffic"}, new String[]{"mcp"}));
            Assertions.assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_REQUESTS_TOTAL,
                    new String[]{"traffic", "outcome", "reason"}, new String[]{"mcp", "rejected", "authentication"}));
            Assertions.assertEquals(7.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_REQUEST_LATENCY + "_sum",
                    new String[]{"traffic", "outcome"}, new String[]{"mcp", "rejected"}));
            metrics.attempt(AiAttemptEvent.Kind.RETRY, AgentRuntimeMetrics.Outcome.TIMEOUT, 5);
            Assertions.assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_AI_ATTEMPTS_TOTAL,
                    new String[]{"kind", "outcome"}, new String[]{"retry", "timeout"}));
            metrics.catalog(false);
            metrics.catalog(true);
            Assertions.assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_CATALOG_OBSERVATIONS_TOTAL,
                    new String[]{"state"}, new String[]{"unavailable"}));
            Assertions.assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_CATALOG_OBSERVATIONS_TOTAL,
                    new String[]{"state"}, new String[]{"available"}));
        } finally {
            MetricsReporter.clean();
            CollectorRegistry.defaultRegistry.clear();
        }
    }

    @Test
    void outstandingRequestDoesNotDecrementAReplacementRegistry() {
        MetricsRegister first = Mockito.mock(MetricsRegister.class);
        MetricsRegister replacement = Mockito.mock(MetricsRegister.class);
        try {
            MetricsReporter.register(first);
            AgentRuntimeMetrics active = MetricsReporter.agentRuntimeMetrics();
            active.started(AgentRuntimeMetrics.Traffic.LLM);
            MetricsReporter.clean();
            Assertions.assertSame(AgentRuntimeMetrics.NOOP, MetricsReporter.agentRuntimeMetrics());
            MetricsReporter.register(replacement);
            active.finished(AgentRuntimeMetrics.Traffic.LLM, AgentRuntimeMetrics.Outcome.CANCELLED, AgentRuntimeMetrics.Reason.NONE, 0);
            Mockito.verify(first, Mockito.never()).gaugeDecrement(ArgumentMatchers.any(), ArgumentMatchers.any());
            Mockito.verify(replacement, Mockito.never()).gaugeDecrement(ArgumentMatchers.any(), ArgumentMatchers.any());
        } finally {
            MetricsReporter.clean();
        }
    }
}
