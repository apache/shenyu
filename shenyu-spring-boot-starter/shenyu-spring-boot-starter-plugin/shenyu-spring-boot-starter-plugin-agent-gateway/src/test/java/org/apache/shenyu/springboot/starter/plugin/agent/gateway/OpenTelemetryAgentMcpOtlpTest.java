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

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import com.sun.net.httpserver.HttpServer;
import io.opentelemetry.api.OpenTelemetry;
import io.opentelemetry.exporter.otlp.http.trace.OtlpHttpSpanExporter;
import io.opentelemetry.sdk.OpenTelemetrySdk;
import io.opentelemetry.sdk.resources.Resource;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.springboot.starter.plugin.tracing.TracingPluginConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.apache.shenyu.plugin.tracing.TracingPlugin;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import reactor.core.publisher.Mono;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpCallTracer;
import org.junit.jupiter.api.Test;
import reactor.util.context.Context;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OpenTelemetryAgentMcpOtlpTest {

    @Test
    void tracingStarterOwnsTheOnlyProviderAndExportsGatewayAndLogicalCallTogether() throws Exception {
        java.util.concurrent.CopyOnWriteArrayList<byte[]> payloads = new java.util.concurrent.CopyOnWriteArrayList<>();
        HttpServer receiver = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        receiver.createContext("/v1/traces", exchange -> {
            payloads.add(exchange.getRequestBody().readAllBytes());
            exchange.getResponseHeaders().set("Content-Type", "application/x-protobuf");
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });
        receiver.start();
        try {
            ShenyuConfig config = new ShenyuConfig();
            config.getTracing().getOtlp().setEndpoint("http://127.0.0.1:" + receiver.getAddress().getPort());
            new ApplicationContextRunner().withUserConfiguration(TracingPluginConfiguration.class, AgentGatewayPluginConfiguration.class)
                    .withBean(ShenyuConfig.class, () -> config)
                    .withPropertyValues("shenyu.tracing.enabled=true", "shenyu.plugins.agent.gateway.enabled=true", "shenyu.plugins.agent.gateway.tracing.enabled=true")
                    .run(context -> {
                        assertTrue(java.util.Objects.isNull(context.getStartupFailure()));
                        assertEquals(1, context.getBeansOfType(SdkTracerProvider.class).size());
                        assertTrue(context.getBeansOfType(OpenTelemetry.class).isEmpty());
                        SdkTracerProvider provider = context.getBean(SdkTracerProvider.class);
                        AgentMcpCallTracer bridge = context.getBean(AgentMcpCallTracer.class);
                        TracingPlugin gateway = (TracingPlugin) context.getBean("tracingPlugin");
                        gateway.execute(MockServerWebExchange.from(MockServerHttpRequest.post("/mcp").build()), exchange -> Mono.deferContextual(view -> {
                            AgentMcpCallTracer.Trace trace = bridge.start(view);
                            io.opentelemetry.context.Context serverContext = view.get(org.apache.shenyu.common.tracing.AgentTraceContext.GATEWAY_REACTOR_CONTEXT_KEY);
                            io.opentelemetry.context.Context logicalContext = trace.attach(Context.empty()).get(io.opentelemetry.context.Context.class);
                            assertEquals(io.opentelemetry.api.trace.Span.fromContext(serverContext).getSpanContext().getTraceId(),
                                    io.opentelemetry.api.trace.Span.fromContext(logicalContext).getSpanContext().getTraceId());
                            trace.finish(Outcome.SUCCESS);
                            trace.finish(Outcome.SUCCESS);
                            return Mono.empty();
                        })).block(Duration.ofSeconds(10));
                        assertTrue(provider.forceFlush().join(10, TimeUnit.SECONDS).isSuccess());
                        assertEquals(1, payloads.size());
                        assertTrue(new String(payloads.get(0), StandardCharsets.UTF_8).contains("shenyu.agent_gateway.tools.call"));
                    });
        } finally {
            receiver.stop(0);
        }
    }

    @Test
    void applicationOwnedSdkExportsOnceOverRealOtlpHttp() throws Exception {
        AtomicReference<byte[]> payload = new AtomicReference<>();
        AtomicReference<String> contentType = new AtomicReference<>();
        AtomicReference<String> method = new AtomicReference<>();
        AtomicInteger requests = new AtomicInteger();
        HttpServer receiver = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        receiver.createContext("/v1/traces", exchange -> {
            requests.incrementAndGet();
            method.set(exchange.getRequestMethod());
            contentType.set(exchange.getRequestHeaders().getFirst("Content-Type"));
            payload.set(exchange.getRequestBody().readAllBytes());
            exchange.getResponseHeaders().set("Content-Type", "application/x-protobuf");
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });
        receiver.start();
        try {
            OtlpHttpSpanExporter exporter = OtlpHttpSpanExporter.builder().setTimeout(Duration.ofSeconds(3))
                    .setEndpoint("http://127.0.0.1:" + receiver.getAddress().getPort() + "/v1/traces").build();
            try (SdkTracerProvider provider = SdkTracerProvider.builder().setResource(Resource.empty())
                    .addSpanProcessor(SimpleSpanProcessor.create(exporter)).build()) {
                OpenTelemetry telemetry = OpenTelemetrySdk.builder().setTracerProvider(provider).build();
                AgentMcpCallTracer.Trace trace = new OpenTelemetryAgentMcpCallTracer(telemetry).start(Context.of("private-subject", "secret-value"));
                trace.finish(Outcome.SUCCESS);
                trace.finish(Outcome.SERVER_ERROR);
                assertTrue(provider.forceFlush().join(5, TimeUnit.SECONDS).isSuccess());
                assertEquals(1, requests.get());
                assertEquals("POST", method.get());
                assertEquals("application/x-protobuf", contentType.get());
                String wire = new String(payload.get(), StandardCharsets.UTF_8);
                assertTrue(wire.contains("shenyu.agent_gateway.tools.call"));
                assertTrue(wire.contains("shenyu.mcp.outcome"));
                assertTrue(wire.contains("success"));
                assertFalse(wire.contains("private-subject"));
                assertFalse(wire.contains("secret-value"));
            }
        } finally {
            receiver.stop(0);
        }
    }
}
