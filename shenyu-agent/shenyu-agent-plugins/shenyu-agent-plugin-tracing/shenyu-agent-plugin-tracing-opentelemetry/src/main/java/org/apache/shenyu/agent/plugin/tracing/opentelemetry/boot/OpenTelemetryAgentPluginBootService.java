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

package org.apache.shenyu.agent.plugin.tracing.opentelemetry.boot;

import io.opentelemetry.sdk.autoconfigure.AutoConfiguredOpenTelemetrySdk;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import org.apache.shenyu.agent.api.config.AgentPluginConfig;
import org.apache.shenyu.agent.api.spi.AgentPluginBootService;
import org.apache.shenyu.spi.Join;

import java.util.Optional;

/**
 * The type OpenTelemetry agent plugin boot service.
 */
@Join
public class OpenTelemetryAgentPluginBootService implements AgentPluginBootService {

    private SdkTracerProvider sdkTracerProvider;

    @Override
    public void start(final AgentPluginConfig agentPluginConfig) {
        agentPluginConfig.getProps().forEach((key, value) -> System.setProperty(String.valueOf(key), String.valueOf(value)));
        AutoConfiguredOpenTelemetrySdk sdk = AutoConfiguredOpenTelemetrySdk.initialize();
        sdkTracerProvider = sdk.getOpenTelemetrySdk().getSdkTracerProvider();
    }

    @Override
    public void close() {
        Optional.ofNullable(sdkTracerProvider).ifPresent(SdkTracerProvider::close);
    }
}
