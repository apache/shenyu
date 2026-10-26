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

package org.apache.shenyu.admin.config.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Operator-configured navigation links from Admin instance diagnostics to existing
 * observability backends (Grafana/Prometheus, logging and tracing platforms).
 *
 * <p>Each template is a plain http/https URL that may reference the following
 * placeholders, whose values are URL-encoded before substitution:
 * {@code {namespace}}, {@code {instanceIp}}, {@code {instancePort}},
 * {@code {instanceType}}, {@code {bootTime}}, {@code {startTime}}, {@code {endTime}},
 * {@code {startTimeIso}} and {@code {endTimeIso}}. Time placeholders are epoch
 * milliseconds; the {@code Iso} variants are ISO-8601 UTC timestamps.</p>
 *
 * <p>Templates must not embed credentials or other sensitive data in the URL;
 * access control of the destination platform stays with that platform. A blank
 * template means the corresponding backend is not configured, which is reported
 * as a distinct state instead of a broken link.</p>
 */
@ConfigurationProperties(prefix = "shenyu.observability")
public class ObservabilityProperties {

    /**
     * URL template of the metrics backend, e.g. a Grafana dashboard or Prometheus query.
     */
    private String metricsUrlTemplate;

    /**
     * URL template of the logging backend, e.g. a Kibana or Loki explore view.
     */
    private String logsUrlTemplate;

    /**
     * URL template of the tracing backend, e.g. a Jaeger or Tempo search view.
     * Exact request lookups require an actual trace ID from a request/log record,
     * so instance diagnostics only links to an instance-scoped search.
     */
    private String traceUrlTemplate;

    /**
     * Gets the metrics URL template.
     *
     * @return metricsUrlTemplate
     */
    public String getMetricsUrlTemplate() {
        return metricsUrlTemplate;
    }

    /**
     * Sets the metrics URL template.
     *
     * @param metricsUrlTemplate metricsUrlTemplate
     */
    public void setMetricsUrlTemplate(final String metricsUrlTemplate) {
        this.metricsUrlTemplate = metricsUrlTemplate;
    }

    /**
     * Gets the logs URL template.
     *
     * @return logsUrlTemplate
     */
    public String getLogsUrlTemplate() {
        return logsUrlTemplate;
    }

    /**
     * Sets the logs URL template.
     *
     * @param logsUrlTemplate logsUrlTemplate
     */
    public void setLogsUrlTemplate(final String logsUrlTemplate) {
        this.logsUrlTemplate = logsUrlTemplate;
    }

    /**
     * Gets the trace URL template.
     *
     * @return traceUrlTemplate
     */
    public String getTraceUrlTemplate() {
        return traceUrlTemplate;
    }

    /**
     * Sets the trace URL template.
     *
     * @param traceUrlTemplate traceUrlTemplate
     */
    public void setTraceUrlTemplate(final String traceUrlTemplate) {
        this.traceUrlTemplate = traceUrlTemplate;
    }
}
