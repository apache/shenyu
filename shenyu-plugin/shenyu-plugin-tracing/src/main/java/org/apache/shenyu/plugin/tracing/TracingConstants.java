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

package org.apache.shenyu.plugin.tracing;

/**
 * The tracing constants.
 */
public final class TracingConstants {

    /**
     * The instrumentation scope name reported for the ShenYu gateway spans.
     */
    public static final String INSTRUMENTATION_NAME = "org.apache.shenyu.plugin.tracing";

    /**
     * The span name used when the ShenYu context is not available, for example on a rejected request.
     */
    public static final String DEFAULT_SPAN_NAME = "shenyu-gateway";

    /**
     * The W3C trace context header.
     */
    public static final String TRACEPARENT = "traceparent";

    /**
     * The exchange attribute that carries the active tracing context of the current request.
     */
    public static final String TRACING_CONTEXT = "shenyuTracingContext";

    /**
     * The Reactor context key that carries the active tracing context across thread switches.
     */
    public static final String REACTOR_CONTEXT_KEY = "org.apache.shenyu.plugin.tracing.context";

    /**
     * The service name resource attribute of the OpenTelemetry SDK.
     */
    public static final String SERVICE_NAME = "service.name";

    private TracingConstants() {
    }
}
