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

import io.opentelemetry.api.trace.Tracer;
import io.opentelemetry.exporter.otlp.http.trace.OtlpHttpSpanExporter;
import io.opentelemetry.exporter.otlp.http.trace.OtlpHttpSpanExporterBuilder;
import io.opentelemetry.sdk.resources.Resource;
import io.opentelemetry.sdk.resources.ResourceBuilder;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.SdkTracerProviderBuilder;
import io.opentelemetry.sdk.trace.SpanProcessor;
import io.opentelemetry.sdk.trace.export.BatchSpanProcessor;
import io.opentelemetry.sdk.trace.export.SpanExporter;
import io.opentelemetry.sdk.trace.samplers.Sampler;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.tracing.TracingConstants;
import org.apache.shenyu.plugin.tracing.TracingPlugin;
import org.apache.shenyu.plugin.tracing.extract.TraceContextExtractor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;

/**
 * The type Tracing plugin configuration.
 *
 * <p>This starter owns the OpenTelemetry SDK, the plugin itself only depends on the API. The global
 * OpenTelemetry is deliberately left untouched, so that a deployment which already runs a Java
 * agent keeps its own instrumentation.</p>
 *
 * <p>The beans are wired by their explicit names, so that an application which brings its own
 * OpenTelemetry beans keeps working. Spans are exported over OTLP when an endpoint is configured;
 * the grpc transport is deliberately not bundled, because grpc netty is already on the classpath of
 * the gateway through the grpc plugin. Nothing of the export path may affect request forwarding, so
 * an exporter which cannot be created is logged and the spans simply stay in the process.</p>
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.tracing.enabled"}, havingValue = "true")
public class TracingPluginConfiguration {

    private static final Logger LOG = LoggerFactory.getLogger(TracingPluginConfiguration.class);

    private static final String DEFAULT_SERVICE_NAME = "shenyu-gateway";

    private static final String TRACES_PATH = "/v1/traces";

    /**
     * The resource of the gateway spans.
     *
     * @param shenyuConfig the shenyu config
     * @return the OpenTelemetry resource
     */
    @Bean
    public Resource tracingResource(final ShenyuConfig shenyuConfig) {
        final ResourceBuilder builder = Resource.getDefault()
                .toBuilder()
                .put(TracingConstants.SERVICE_NAME, serviceName(shenyuConfig));
        resourceAttributes(shenyuConfig).ifPresent(attributes -> attributes.stringPropertyNames()
                .forEach(name -> builder.put(name, attributes.getProperty(name))));
        return builder.build();
    }

    /**
     * The exporter of the gateway spans, which the context closes after it has closed the tracer
     * provider.
     *
     * @param shenyuConfig the shenyu config
     * @return the exporter holder
     */
    @Bean
    public TracingExporterHolder tracingExporterHolder(final ShenyuConfig shenyuConfig) {
        return new TracingExporterHolder(createSpanExporter(shenyuConfig).orElse(null));
    }

    /**
     * The tracer provider of the gateway spans.
     *
     * @param resource the OpenTelemetry resource
     * @param exporterHolder the exporter of the gateway spans
     * @param shenyuConfig the shenyu config
     * @return the sdk tracer provider
     */
    @Bean(destroyMethod = "close")
    public SdkTracerProvider tracingTracerProvider(@Qualifier("tracingResource") final Resource resource,
                                                   final TracingExporterHolder exporterHolder,
                                                   final ShenyuConfig shenyuConfig) {
        final Sampler sampler = sampler(shenyuConfig);
        final SdkTracerProviderBuilder builder = SdkTracerProvider.builder()
                .setResource(resource)
                .setSampler(sampler);
        final Optional<SpanProcessor> processor = exporterHolder.exporter().flatMap(this::spanProcessor);
        processor.ifPresent(builder::addSpanProcessor);
        // the processor of the exporter may fail to be created, so the log reports the export from
        // the processor which is really attached instead of claiming that the spans are exported
        LOG.info("shenyu tracing: service name {}, sampler {}, span creation {}, spans exported {}",
                serviceName(shenyuConfig), sampler.getDescription(),
                createSpan(shenyuConfig) ? "enabled" : "disabled",
                processor.isPresent() ? endpointOf(shenyuConfig) : "disabled");
        return builder.build();
    }

    /**
     * The tracer of the gateway spans.
     *
     * @param tracerProvider the tracer provider
     * @return the tracer
     */
    @Bean
    public Tracer tracingTracer(@Qualifier("tracingTracerProvider") final SdkTracerProvider tracerProvider) {
        return tracerProvider.get(TracingConstants.INSTRUMENTATION_NAME);
    }

    /**
     * Tracing plugin.
     *
     * @param tracer the tracer
     * @param shenyuConfig the shenyu config
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin tracingPlugin(@Qualifier("tracingTracer") final Tracer tracer,
                                      final ShenyuConfig shenyuConfig) {
        return new TracingPlugin(tracer, new TraceContextExtractor(), createSpan(shenyuConfig));
    }

    /**
     * Acquires the tracing config.
     *
     * @param shenyuConfig the shenyu config
     * @return the tracing config, empty while it is not configured
     */
    private Optional<ShenyuConfig.TracingConfig> tracingConfig(final ShenyuConfig shenyuConfig) {
        return Optional.ofNullable(shenyuConfig.getTracing());
    }

    /**
     * Acquires the service name of the gateway.
     *
     * @param shenyuConfig the shenyu config
     * @return the service name
     */
    private String serviceName(final ShenyuConfig shenyuConfig) {
        return tracingConfig(shenyuConfig)
                .map(ShenyuConfig.TracingConfig::getServiceName)
                .filter(StringUtils::isNotBlank)
                .orElse(DEFAULT_SERVICE_NAME);
    }

    /**
     * Tells whether the gateway creates a span of its own.
     *
     * @param shenyuConfig the shenyu config
     * @return true while the plugin creates a span
     */
    private boolean createSpan(final ShenyuConfig shenyuConfig) {
        return tracingConfig(shenyuConfig).map(ShenyuConfig.TracingConfig::getCreateSpan).orElse(Boolean.TRUE);
    }

    /**
     * Acquires the resource attributes which are added to the resource of the gateway spans.
     *
     * @param shenyuConfig the shenyu config
     * @return the configured resource attributes, empty while none is configured
     */
    private Optional<Properties> resourceAttributes(final ShenyuConfig shenyuConfig) {
        return tracingConfig(shenyuConfig)
                .map(ShenyuConfig.TracingConfig::getResourceAttributes)
                .filter(attributes -> !attributes.isEmpty());
    }

    /**
     * Builds the sampler which decides whether a request is traced.
     *
     * @param shenyuConfig the shenyu config
     * @return the sampler
     */
    private Sampler sampler(final ShenyuConfig shenyuConfig) {
        final ShenyuConfig.TracingConfig config = tracingConfig(shenyuConfig)
                .orElseGet(ShenyuConfig.TracingConfig::new);
        final String sampler = Optional.ofNullable(config.getSampler())
                .map(String::trim)
                .filter(StringUtils::isNotBlank)
                .orElse(TracingConstants.SAMPLER_PARENT_BASED)
                .toLowerCase(Locale.ROOT);
        switch (sampler) {
            case TracingConstants.SAMPLER_ALWAYS_ON:
                return Sampler.alwaysOn();
            case TracingConstants.SAMPLER_ALWAYS_OFF:
                return Sampler.alwaysOff();
            case TracingConstants.SAMPLER_RATIO:
                return Sampler.parentBased(Sampler.traceIdRatioBased(ratio(config.getSampleRatio())));
            case TracingConstants.SAMPLER_PARENT_BASED:
                return Sampler.parentBased(Sampler.alwaysOn());
            default:
                LOG.warn("shenyu tracing does not know the sampler {}, the parent based sampler is used", sampler);
                return Sampler.parentBased(Sampler.alwaysOn());
        }
    }

    /**
     * Normalizes the sample ratio.
     *
     * <p>A ratio which is not a number or which is out of the range would silently change the
     * sampling behaviour, so it is reported and clamped instead of failing the gateway.</p>
     *
     * @param sampleRatio the configured sample ratio
     * @return the ratio within the valid range
     */
    private double ratio(final double sampleRatio) {
        if (Double.isNaN(sampleRatio)) {
            LOG.warn("shenyu tracing could not read the sample ratio, no request is sampled");
            return 0.0D;
        }
        final double bounded = Math.max(0.0D, Math.min(1.0D, sampleRatio));
        if (bounded != sampleRatio) {
            LOG.warn("shenyu tracing clamps the sample ratio {} to {}", sampleRatio, bounded);
        }
        return bounded;
    }

    /**
     * Builds the batch processor of the exporter.
     *
     * @param exporter the exporter of the gateway spans
     * @return the batch processor, empty while it cannot be created
     */
    private Optional<SpanProcessor> spanProcessor(final SpanExporter exporter) {
        try {
            return Optional.of(BatchSpanProcessor.builder(exporter).build());
        } catch (RuntimeException | LinkageError ex) {
            LOG.warn("shenyu tracing could not create the batch processor of the exporter, the spans are not exported", ex);
            return Optional.empty();
        }
    }

    /**
     * Creates the OTLP exporter of the gateway spans.
     *
     * @param shenyuConfig the shenyu config
     * @return the span exporter, empty while no endpoint is configured or the exporter cannot be created
     */
    private Optional<SpanExporter> createSpanExporter(final ShenyuConfig shenyuConfig) {
        final Optional<ShenyuConfig.TracingConfig.OtlpConfig> configured = tracingConfig(shenyuConfig)
                .map(ShenyuConfig.TracingConfig::getOtlp)
                .filter(otlp -> StringUtils.isNotBlank(otlp.getEndpoint()));
        if (!configured.isPresent()) {
            return Optional.empty();
        }
        final ShenyuConfig.TracingConfig.OtlpConfig otlp = configured.get();
        try {
            final String endpoint = endpoint(otlp.getEndpoint());
            final OtlpHttpSpanExporterBuilder builder = OtlpHttpSpanExporter.builder().setEndpoint(endpoint);
            if (Objects.nonNull(otlp.getTimeout())) {
                builder.setTimeout(otlp.getTimeout());
            }
            if (Objects.nonNull(otlp.getHeaders())) {
                final Properties headers = otlp.getHeaders();
                headers.stringPropertyNames().forEach(name -> builder.addHeader(name, headers.getProperty(name)));
            }
            return Optional.of(builder.build());
        } catch (RuntimeException | LinkageError ex) {
            // the message of the failure may quote the endpoint, and an endpoint may carry a token in
            // its user information or its query, so neither the throwable nor its message is logged,
            // at any level: only the redacted endpoint and the type of the failure.
            LOG.warn("shenyu tracing could not create the OTLP exporter of {}, the spans are not exported: {}",
                    redactedEndpoint(otlp.getEndpoint()), ex.getClass().getSimpleName());
            return Optional.empty();
        }
    }

    /**
     * Resolves the traces endpoint: a base endpoint without a path gets the traces path appended, so
     * that a collector is not called at its root and the spans do not get lost silently.
     *
     * @param configuredEndpoint the configured endpoint
     * @return the endpoint of the traces of the collector
     */
    private String endpoint(final String configuredEndpoint) {
        final String endpoint = configuredEndpoint.trim();
        final String path = URI.create(endpoint).getPath();
        if (StringUtils.isNotBlank(path) && !"/".equals(path)) {
            return endpoint;
        }
        // the path is appended instead of concatenated, so that a query or a fragment of the
        // configured endpoint keeps being valid
        return UriComponentsBuilder.fromUriString(endpoint).path(TRACES_PATH).build().toUriString();
    }

    /**
     * Acquires the endpoint the spans are exported to, without the credentials it may carry.
     *
     * @param shenyuConfig the shenyu config
     * @return the endpoint, disabled while the spans are not exported
     */
    private String endpointOf(final ShenyuConfig shenyuConfig) {
        return tracingConfig(shenyuConfig)
                .map(ShenyuConfig.TracingConfig::getOtlp)
                .map(ShenyuConfig.TracingConfig.OtlpConfig::getEndpoint)
                .filter(StringUtils::isNotBlank)
                .map(this::endpoint)
                .map(TracingPluginConfiguration::redactedEndpoint)
                .orElse("disabled");
    }

    /**
     * Removes the user information, the query and the fragment from an endpoint.
     *
     * <p>A collector endpoint commonly carries a token in the query or in the user information, and
     * the log must not expose it.</p>
     *
     * @param endpoint the endpoint
     * @return the endpoint without its credentials
     */
    static String redactedEndpoint(final String endpoint) {
        try {
            return UriComponentsBuilder.fromUriString(endpoint)
                    .userInfo(null)
                    .query(null)
                    .fragment(null)
                    .build()
                    .toUriString();
        } catch (RuntimeException ex) {
            return "the configured endpoint";
        }
    }
}
