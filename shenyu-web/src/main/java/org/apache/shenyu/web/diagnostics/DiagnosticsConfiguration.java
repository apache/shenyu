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

package org.apache.shenyu.web.diagnostics;

import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

/**
 * Wires the opt-in gateway diagnostic bundle facility.
 *
 * <p>The whole configuration is gated on {@code shenyu.diagnostics.enabled=true}; a default
 * deployment creates none of these beans and the endpoint is not registered. Collector order is
 * fixed here so the manifest lists sections deterministically.</p>
 */
@Configuration
@ConditionalOnProperty(name = "shenyu.diagnostics.enabled", havingValue = "true")
@EnableConfigurationProperties(DiagnosticProperties.class)
public class DiagnosticsConfiguration {

    /**
     * The shared redaction policy.
     *
     * @return the redactor
     */
    @Bean
    public DiagnosticRedactor diagnosticRedactor() {
        return new DiagnosticRedactor();
    }

    /**
     * The archive writer.
     *
     * @param properties the diagnostic bounds
     * @return the writer
     */
    @Bean
    public DiagnosticBundleWriter diagnosticBundleWriter(final DiagnosticProperties properties) {
        return new DiagnosticBundleWriter(properties);
    }

    /**
     * The bundle service composing the fixed collector set.
     *
     * @param properties the diagnostic bounds
     * @param redactor   the shared redaction policy
     * @param writer     the archive writer
     * @param webHandler the gateway web handler
     * @return the bundle service
     */
    @Bean
    public DiagnosticBundleService diagnosticBundleService(final DiagnosticProperties properties, final DiagnosticRedactor redactor,
                                                           final DiagnosticBundleWriter writer, final ShenyuWebHandler webHandler) {
        List<SectionCollector> collectors = List.of(
                new RuntimeInfoCollector(),
                new PluginInventoryCollector(webHandler),
                new EffectiveConfigDigestCollector(redactor));
        return new DiagnosticBundleService(collectors, properties, writer);
    }
}
