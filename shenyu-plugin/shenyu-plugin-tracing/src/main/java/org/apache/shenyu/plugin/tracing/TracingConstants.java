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
     * The W3C trace state header.
     */
    public static final String TRACESTATE = "tracestate";

    /**
     * The sampler which continues the decision of the parent and samples the request otherwise.
     */
    public static final String SAMPLER_PARENT_BASED = "parent_based";

    /**
     * The sampler which samples every request.
     */
    public static final String SAMPLER_ALWAYS_ON = "always_on";

    /**
     * The sampler which samples no request.
     */
    public static final String SAMPLER_ALWAYS_OFF = "always_off";

    /**
     * The sampler which samples the given ratio of the requests.
     */
    public static final String SAMPLER_RATIO = "ratio";

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

    /**
     * The http request method attribute, the key of the OpenTelemetry semantic convention.
     */
    public static final String HTTP_REQUEST_METHOD = "http.request.method";

    /**
     * The http response status code attribute, the key of the OpenTelemetry semantic convention.
     */
    public static final String HTTP_RESPONSE_STATUS_CODE = "http.response.status_code";

    /**
     * The matched route of the request, the key of the OpenTelemetry semantic convention.
     */
    public static final String HTTP_ROUTE = "http.route";

    /**
     * The host of the upstream which served the request. The server of a gateway span is the gateway
     * itself, so the upstream is recorded with a gateway specific key.
     */
    public static final String UPSTREAM_HOST = "shenyu.upstream.host";

    /**
     * The rpc type of the request.
     */
    public static final String RPC_TYPE = "shenyu.rpc.type";

    /**
     * The module of the matched route, the application name of the upstream in most protocols.
     */
    public static final String ROUTE_MODULE = "shenyu.route.module";

    /**
     * The method of the matched route, the upstream service or method of the protocol.
     */
    public static final String ROUTE_METHOD = "shenyu.route.method";

    /**
     * The context path of the matched route.
     */
    public static final String CONTEXT_PATH = "shenyu.context.path";

    /**
     * The selector id of the routing decision, when the plugin of the protocol exposes it.
     */
    public static final String SELECTOR_ID = "shenyu.selector.id";

    /**
     * The error type of a request which is an error span, the timeout outcome or the exception type.
     */
    public static final String ERROR_TYPE = "shenyu.error.type";

    /**
     * The error type of a request which ended with a timeout.
     */
    public static final String ERROR_TYPE_TIMEOUT = "timeout";

    /**
     * The attribute which marks a request that the client cancelled.
     */
    public static final String CANCELLED = "shenyu.cancelled";

    private TracingConstants() {
    }
}
