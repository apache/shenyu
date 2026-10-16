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

package org.apache.shenyu.springboot.starter.plugin.tracing;

import io.opentelemetry.api.common.AttributeKey;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.Tracer;
import io.opentelemetry.api.trace.TracerProvider;
import io.opentelemetry.sdk.resources.Resource;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.tracing.TracingConstants;
import org.apache.shenyu.plugin.tracing.TracingPlugin;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

/**
 * The Test Case For TracingPluginConfiguration.
 */
public class TracingPluginConfigurationTest {

    private final ApplicationContextRunner contextRunner = new ApplicationContextRunner()
            .withUserConfiguration(TracingPluginConfiguration.class)
            .withBean(ShenyuConfig.class, ShenyuConfig::new);

    @Test
    public void testTracingIsDisabledByDefault() {
        contextRunner.run(context -> {
            Assertions.assertFalse(context.containsBean("tracingPlugin"));
            Assertions.assertFalse(context.containsBean("tracingTracer"));
            Assertions.assertFalse(context.containsBean("tracingTracerProvider"));
        });
    }

    @Test
    public void testTracingIsDisabledByExplicitFalse() {
        contextRunner.withPropertyValues("shenyu.tracing.enabled=false").run(context -> {
            Assertions.assertFalse(context.containsBean("tracingPlugin"));
            Assertions.assertFalse(context.containsBean("tracingTracer"));
            Assertions.assertFalse(context.containsBean("tracingTracerProvider"));
        });
    }

    @Test
    public void testBeansAreRegisteredWhenTracingIsEnabled() {
        contextRunner.withPropertyValues("shenyu.tracing.enabled=true").run(context -> {
            Assertions.assertTrue(context.containsBean("tracingPlugin"));
            Assertions.assertTrue(context.containsBean("tracingTracer"));
            Assertions.assertTrue(context.containsBean("tracingTracerProvider"));
            Assertions.assertTrue(context.containsBean("tracingResource"));
            Assertions.assertInstanceOf(TracingPlugin.class, context.getBean(ShenyuPlugin.class));
            Assertions.assertNotNull(context.getBean(Tracer.class));
            Assertions.assertNotNull(context.getBean(SdkTracerProvider.class));
        });
    }

    @Test
    public void testTracerProviderRecordsSpansWithoutAnExporter() {
        contextRunner.withPropertyValues("shenyu.tracing.enabled=true").run(context -> {
            Span span = context.getBean(Tracer.class).spanBuilder("test").startSpan();
            try {
                // no exporter is wired yet, the spans still have to carry a usable trace id so that
                // the logging plugins can correlate the access log with the trace.
                Assertions.assertTrue(span.getSpanContext().isValid());
            } finally {
                span.end();
            }
        });
    }

    @Test
    public void testExistingOpenTelemetryBeansDoNotBreakTheWiring() {
        Tracer otherTracer = TracerProvider.noop().get("other.instrumentation");
        new ApplicationContextRunner()
                .withUserConfiguration(TracingPluginConfiguration.class)
                .withBean(ShenyuConfig.class, ShenyuConfig::new)
                .withBean("otherTracer", Tracer.class, () -> otherTracer)
                .withBean("otherTracerProvider", SdkTracerProvider.class, () -> SdkTracerProvider.builder().build())
                .withPropertyValues("shenyu.tracing.enabled=true")
                .run(context -> {
                    // an application which already brings its own OpenTelemetry beans must not make
                    // the gateway wiring ambiguous, every bean is resolved by its explicit name.
                    Assertions.assertSame(otherTracer, context.getBean("otherTracer", Tracer.class));
                    Assertions.assertNotSame(otherTracer, context.getBean("tracingTracer", Tracer.class));
                    Assertions.assertInstanceOf(TracingPlugin.class, context.getBean(ShenyuPlugin.class));
                });
    }

    @Test
    public void testDefaultServiceNameIsUsed() {
        contextRunner.withPropertyValues("shenyu.tracing.enabled=true").run(context -> Assertions.assertEquals(
                "shenyu-gateway", serviceName(context.getBean(Resource.class))));
    }

    @Test
    public void testConfiguredServiceNameIsUsed() {
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.getTracing().setServiceName("shenyu-test");
        new ApplicationContextRunner()
                .withUserConfiguration(TracingPluginConfiguration.class)
                .withBean(ShenyuConfig.class, () -> shenyuConfig)
                .withPropertyValues("shenyu.tracing.enabled=true")
                .run(context -> Assertions.assertEquals("shenyu-test", serviceName(context.getBean(Resource.class))));
    }

    private String serviceName(final Resource resource) {
        return resource.getAttribute(AttributeKey.stringKey(TracingConstants.SERVICE_NAME));
    }
}
