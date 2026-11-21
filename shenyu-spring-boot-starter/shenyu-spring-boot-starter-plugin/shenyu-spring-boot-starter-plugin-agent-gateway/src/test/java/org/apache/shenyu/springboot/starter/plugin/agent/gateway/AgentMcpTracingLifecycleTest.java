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

import io.opentelemetry.api.OpenTelemetry;
import io.opentelemetry.sdk.OpenTelemetrySdk;
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpCallTracer;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.support.GenericApplicationContext;
import reactor.util.context.Context;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Application SDK ownership across independent plugin contexts and reloads. */
class AgentMcpTracingLifecycleTest {

    @Test
    void unloadingAndRecreatingPluginDoesNotCloseOrDuplicateApplicationSdk() {
        InMemorySpanExporter exporter = InMemorySpanExporter.create();
        try (SdkTracerProvider provider = SdkTracerProvider.builder().addSpanProcessor(SimpleSpanProcessor.create(exporter)).build();
                GenericApplicationContext application = new GenericApplicationContext()) {
            OpenTelemetry telemetry = OpenTelemetrySdk.builder().setTracerProvider(provider).build();
            application.registerBean(OpenTelemetry.class, () -> telemetry);
            application.refresh();
            ApplicationContextRunner runner = new ApplicationContextRunner().withParent(application)
                    .withUserConfiguration(AgentGatewayPluginConfiguration.class)
                    .withPropertyValues("shenyu.plugins.agent.gateway.enabled=true");
            AgentMcpCallTracer[] previous = new AgentMcpCallTracer[1];
            for (boolean enabled : List.of(true, false, true)) {
                int before = exporter.getFinishedSpanItems().size();
                runner.withPropertyValues("shenyu.plugins.agent.gateway.tracing.enabled=" + enabled).run(context -> {
                    assertNull(context.getStartupFailure());
                    assertEquals(0, context.getBeanFactory().getBeanNamesForType(SdkTracerProvider.class).length);
                    if (enabled) {
                        AgentMcpCallTracer tracer = context.getBean(AgentMcpCallTracer.class);
                        assertNotSame(previous[0], tracer);
                        previous[0] = tracer;
                        AgentMcpCallTracer.Trace trace = tracer.start(Context.empty());
                        trace.finish(AgentMcpCallObserver.Outcome.SUCCESS);
                        trace.finish(AgentMcpCallObserver.Outcome.SERVER_ERROR);
                    } else {
                        assertFalse(context.containsBean("agentGatewayCallTracer"));
                    }
                });
                assertEquals(before + (enabled ? 1 : 0), exporter.getFinishedSpanItems().size());
                var span = telemetry.getTracer("application-owner").spanBuilder("after-plugin-close").startSpan();
                assertTrue(span.isRecording());
                span.end();
                assertEquals(before + (enabled ? 2 : 1), exporter.getFinishedSpanItems().size());
            }
            assertEquals(2, exporter.getFinishedSpanItems().stream().filter(span -> "shenyu.agent_gateway.tools.call".equals(span.getName())).count());
        }
    }
}
