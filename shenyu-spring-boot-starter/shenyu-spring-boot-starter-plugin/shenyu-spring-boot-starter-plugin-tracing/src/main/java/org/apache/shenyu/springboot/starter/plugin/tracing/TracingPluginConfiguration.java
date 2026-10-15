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
import io.opentelemetry.sdk.resources.Resource;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.samplers.Sampler;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.tracing.TracingConstants;
import org.apache.shenyu.plugin.tracing.TracingPlugin;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Optional;

/**
 * The type Tracing plugin configuration.
 *
 * <p>This starter owns the OpenTelemetry SDK, the plugin itself only depends on the API. The global
 * OpenTelemetry is deliberately left untouched, so that a deployment which already runs a Java
 * agent keeps its own instrumentation.</p>
 *
 * <p>The beans are wired by their explicit names, so that an application which brings its own
 * OpenTelemetry beans keeps working. Span export is added by a follow-up change, this configuration
 * already produces spans with a usable trace id, which is what the access log correlation needs.</p>
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.tracing.enabled"}, havingValue = "true")
public class TracingPluginConfiguration {

    private static final String DEFAULT_SERVICE_NAME = "shenyu-gateway";

    /**
     * The resource of the gateway spans.
     *
     * @param shenyuConfig the shenyu config
     * @return the OpenTelemetry resource
     */
    @Bean
    public Resource tracingResource(final ShenyuConfig shenyuConfig) {
        return Resource.getDefault()
                .toBuilder()
                .put(TracingConstants.SERVICE_NAME, serviceName(shenyuConfig))
                .build();
    }

    /**
     * The tracer provider of the gateway spans.
     *
     * @param resource the OpenTelemetry resource
     * @return the sdk tracer provider
     */
    @Bean(destroyMethod = "close")
    public SdkTracerProvider tracingTracerProvider(@Qualifier("tracingResource") final Resource resource) {
        return SdkTracerProvider.builder()
                .setResource(resource)
                .setSampler(Sampler.parentBased(Sampler.alwaysOn()))
                .build();
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
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin tracingPlugin(@Qualifier("tracingTracer") final Tracer tracer) {
        return new TracingPlugin(tracer);
    }

    private String serviceName(final ShenyuConfig shenyuConfig) {
        return Optional.ofNullable(shenyuConfig.getTracing())
                .map(ShenyuConfig.TracingConfig::getServiceName)
                .filter(StringUtils::isNotBlank)
                .orElse(DEFAULT_SERVICE_NAME);
    }
}
