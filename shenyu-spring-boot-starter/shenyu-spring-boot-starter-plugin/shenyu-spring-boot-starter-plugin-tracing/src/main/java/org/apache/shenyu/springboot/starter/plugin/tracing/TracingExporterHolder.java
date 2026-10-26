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

import io.opentelemetry.sdk.trace.export.SpanExporter;
import org.springframework.beans.factory.DisposableBean;

import java.util.Objects;
import java.util.Optional;

/**
 * Holds the OTLP exporter of the gateway spans and closes it together with the application context.
 *
 * <p>The batch processor of the tracer provider does not close the exporter it writes to, while the
 * OTLP exporter keeps a connection pool. The exporter therefore is owned by a bean of its own, which
 * the context closes after it has closed the tracer provider.</p>
 */
final class TracingExporterHolder implements DisposableBean {

    private final SpanExporter exporter;

    /**
     * Instantiates a new tracing exporter holder.
     *
     * @param exporter the exporter, null while the spans are not exported
     */
    TracingExporterHolder(final SpanExporter exporter) {
        this.exporter = exporter;
    }

    /**
     * Acquires the exporter.
     *
     * @return the exporter, empty while the spans are not exported
     */
    Optional<SpanExporter> exporter() {
        return Optional.ofNullable(exporter);
    }

    @Override
    public void destroy() {
        if (Objects.nonNull(exporter)) {
            exporter.close();
        }
    }
}
