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

import io.opentelemetry.api.GlobalOpenTelemetry;
import io.opentelemetry.api.common.AttributeKey;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.Tracer;
import io.opentelemetry.api.trace.TracerProvider;
import io.opentelemetry.sdk.resources.Resource;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.tracing.TracingConstants;
import org.apache.shenyu.plugin.tracing.TracingPlugin;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.boot.context.properties.bind.Bindable;
import org.springframework.boot.context.properties.bind.Binder;
import org.springframework.boot.context.properties.source.MapConfigurationPropertySource;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import com.sun.net.httpserver.HttpServer;
import org.awaitility.Awaitility;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.time.Duration;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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

    @Test
    public void testAlwaysOffSamplerDoesNotRecord() {
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.getTracing().setSampler(TracingConstants.SAMPLER_ALWAYS_OFF);
        runner(shenyuConfig).run(context -> Assertions.assertFalse(
                context.getBean(Tracer.class).spanBuilder("test").startSpan().isRecording()));
    }

    @Test
    public void testRatioSampler() {
        ShenyuConfig withoutSamples = new ShenyuConfig();
        withoutSamples.getTracing().setSampler(TracingConstants.SAMPLER_RATIO);
        withoutSamples.getTracing().setSampleRatio(0.0D);
        runner(withoutSamples).run(context -> Assertions.assertFalse(
                context.getBean(Tracer.class).spanBuilder("test").startSpan().isRecording()));
        ShenyuConfig withSamples = new ShenyuConfig();
        withSamples.getTracing().setSampler(TracingConstants.SAMPLER_RATIO);
        withSamples.getTracing().setSampleRatio(1.0D);
        runner(withSamples).run(context -> Assertions.assertTrue(
                context.getBean(Tracer.class).spanBuilder("test").startSpan().isRecording()));
    }

    @Test
    public void testTheGlobalOpenTelemetryIsNotReplaced() {
        // a deployment which instruments the gateway already, for example with a java agent, keeps
        // its own global OpenTelemetry: the tracer of the global never records our spans
        contextRunner.withPropertyValues("shenyu.tracing.enabled=true").run(context ->
                Assertions.assertFalse(GlobalOpenTelemetry.getTracer("test")
                        .spanBuilder("test").startSpan().isRecording()));
    }

    @Test
    public void testAnExporterWithAnInvalidEndpointDoesNotFailTheStartup() {
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.getTracing().getOtlp().setEndpoint("not a valid endpoint");
        runner(shenyuConfig).run(context -> {
            Assertions.assertNull(context.getStartupFailure());
            Assertions.assertTrue(context.containsBean("tracingPlugin"));
        });
    }

    @Test
    public void testAnUnreachableExporterDoesNotAffectTheSpan() {
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.getTracing().getOtlp().setEndpoint("http://127.0.0.1:1/v1/traces");
        runner(shenyuConfig).run(context -> {
            Span span = context.getBean(Tracer.class).spanBuilder("test").startSpan();
            Assertions.assertDoesNotThrow(() -> {
                span.end();
            });
        });
    }

    @Test
    public void testResourceAttributesAreRecorded() {
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        Properties attributes = new Properties();
        attributes.setProperty("deployment.environment", "test");
        shenyuConfig.getTracing().setResourceAttributes(attributes);
        new ApplicationContextRunner()
                .withUserConfiguration(TracingPluginConfiguration.class)
                .withBean(ShenyuConfig.class, () -> shenyuConfig)
                .withPropertyValues("shenyu.tracing.enabled=true")
                .run(context -> Assertions.assertEquals("test", context.getBean(Resource.class)
                        .getAttribute(AttributeKey.stringKey("deployment.environment"))));
    }

    @Test
    public void testTheTracingConfigIsBoundFromProperties() {
        MapConfigurationPropertySource source = new MapConfigurationPropertySource(Map.of(
                "shenyu.tracing.enabled", "true",
                "shenyu.tracing.create-span", "false",
                "shenyu.tracing.sampler", "ratio",
                "shenyu.tracing.sample-ratio", "0.25",
                "shenyu.tracing.otlp.endpoint", "http://collector:4318/v1/traces",
                "shenyu.tracing.otlp.headers.authorization", "Bearer test"));
        Binder binder = new Binder(source);
        ShenyuConfig.TracingConfig config = binder.bind("shenyu.tracing", Bindable.of(ShenyuConfig.TracingConfig.class))
                .orElseGet(ShenyuConfig.TracingConfig::new);
        Assertions.assertFalse(config.getCreateSpan());
        Assertions.assertEquals("ratio", config.getSampler());
        Assertions.assertEquals(0.25D, config.getSampleRatio());
        Assertions.assertEquals("http://collector:4318/v1/traces", config.getOtlp().getEndpoint());
        Assertions.assertEquals("Bearer test", config.getOtlp().getHeaders().getProperty("authorization"));
    }

    @Test
    public void testSpansAreExportedToTheCollector() throws IOException {
        AtomicReference<String> exported = new AtomicReference<>();
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/v1/traces", exchange -> {
            try (InputStream body = exchange.getRequestBody()) {
                body.readAllBytes();
            }
            exported.set(exchange.getRequestURI().getPath());
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });
        server.start();
        try {
            ShenyuConfig shenyuConfig = new ShenyuConfig();
            // the configured endpoint is the base endpoint of the collector, the traces path is
            // appended by the configuration
            shenyuConfig.getTracing().getOtlp().setEndpoint("http://127.0.0.1:" + server.getAddress().getPort());
            runner(shenyuConfig).run(context -> {
                // the span is written to the collector when the context closes, which flushes the
                // batch of the exporter
                Span span = context.getBean(Tracer.class).spanBuilder("test").startSpan();
                span.end();
            });
            Awaitility.await().atMost(Duration.ofSeconds(10)).until(() -> Objects.nonNull(exported.get()));
        } finally {
            server.stop(0);
        }
        Assertions.assertEquals("/v1/traces", exported.get());
    }

    @Test
    public void testTheExporterIsOwnedByTheContext() {
        ShenyuConfig exported = new ShenyuConfig();
        exported.getTracing().getOtlp().setEndpoint("http://127.0.0.1:4318/v1/traces");
        runner(exported).run(context -> Assertions.assertTrue(
                context.getBean(TracingExporterHolder.class).exporter().isPresent()));
        runner(new ShenyuConfig()).run(context -> Assertions.assertFalse(
                context.getBean(TracingExporterHolder.class).exporter().isPresent()));
    }

    @Test
    public void testTheChainCompletesWhileTheExporterIsUnreachable() {
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.getTracing().getOtlp().setEndpoint("http://127.0.0.1:1/v1/traces");
        runner(shenyuConfig).run(context -> {
            ServerWebExchange exchange = MockServerWebExchange.from(
                    MockServerHttpRequest.get("/http/order").build());
            exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
            ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
            when(chain.execute(any())).thenReturn(Mono.empty());
            // the export of the span fails in the background, the request is forwarded anyway
            Assertions.assertDoesNotThrow(() -> context.getBean(ShenyuPlugin.class)
                    .execute(exchange, chain)
                    .block(Duration.ofSeconds(5)));
            verify(chain).execute(exchange);
        });
    }

    @Test
    public void testTheLoggedEndpointDoesNotCarryCredentials() {
        Assertions.assertEquals("http://host:4318/v1/traces", TracingPluginConfiguration
                .redactedEndpoint("http://user:pass@host:4318/v1/traces?token=secret"));
        Assertions.assertEquals("http://host:4318/v1/traces",
                TracingPluginConfiguration.redactedEndpoint("http://host:4318/v1/traces#fragment"));
        Assertions.assertDoesNotThrow(() ->
                TracingPluginConfiguration.redactedEndpoint("not a valid endpoint"));
    }

    private ApplicationContextRunner runner(final ShenyuConfig shenyuConfig) {
        return new ApplicationContextRunner()
                .withUserConfiguration(TracingPluginConfiguration.class)
                .withBean(ShenyuConfig.class, () -> shenyuConfig)
                .withPropertyValues("shenyu.tracing.enabled=true");
    }
}
