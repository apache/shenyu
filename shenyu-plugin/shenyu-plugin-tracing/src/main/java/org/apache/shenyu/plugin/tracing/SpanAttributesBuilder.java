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

import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.StatusCode;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.SignalType;

import java.net.URI;
import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.TimeoutException;

/**
 * Builds the name and the attributes of a ShenYu gateway span.
 *
 * <p>The builder only records bounded values: the request path and the query string are left out, so
 * that neither the attributes nor the span name grow with the traffic. Route information is taken
 * from the metadata of the matched api or from the context path of the gateway, not from the raw
 * request path.</p>
 *
 * <p>The status description of a failure is limited to its type, which is a bounded value. The
 * failure itself is recorded with the OpenTelemetry convention, whose event carries the type, the
 * message and the stack trace of the exception.</p>
 */
public final class SpanAttributesBuilder {

    private static final int MAX_CAUSE_DEPTH = 16;

    private static final String TIMEOUT_NAME_FRAGMENT = "timeout";

    private SpanAttributesBuilder() {
    }

    /**
     * Builds the span name of the request from the http method and the matched route.
     *
     * <p>This follows the OpenTelemetry http conventions: the method plus the route. A request which
     * did not match a route is named by its method only, so the name never grows with the path.</p>
     *
     * @param exchange the current exchange
     * @return the span name
     */
    public static String spanName(final ServerWebExchange exchange) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        final String method = Objects.isNull(shenyuContext) ? null : shenyuContext.getHttpMethod();
        final String route = route(exchange);
        if (StringUtils.isBlank(route)) {
            return StringUtils.isBlank(method) ? TracingConstants.DEFAULT_SPAN_NAME : method;
        }
        return StringUtils.isBlank(method) ? route : method + " " + route;
    }

    /**
     * Records the request information which is known before the remaining chain runs.
     *
     * @param exchange the current exchange
     * @param span the gateway span of the request
     */
    public static void recordRequest(final ServerWebExchange exchange, final Span span) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        if (Objects.isNull(shenyuContext)) {
            return;
        }
        setAttribute(span, TracingConstants.HTTP_REQUEST_METHOD, shenyuContext.getHttpMethod());
        setAttribute(span, TracingConstants.RPC_TYPE, shenyuContext.getRpcType());
        setAttribute(span, TracingConstants.ROUTE_MODULE, shenyuContext.getModule());
        setAttribute(span, TracingConstants.HTTP_ROUTE, route(exchange));
        // the decorators of the http like protocols store the raw request path in the method field,
        // recording it would create an attribute per request path, so it is skipped for them.
        if (!Objects.equals(shenyuContext.getMethod(), shenyuContext.getPath())) {
            setAttribute(span, TracingConstants.ROUTE_METHOD, shenyuContext.getMethod());
        }
    }

    /**
     * Records the outcome of the request, which is only known once the remaining chain has run.
     *
     * <p>The status code of a failed request is taken from the exception when the gateway decided it
     * there, otherwise it is written by the error handler after this span has ended and stays
     * unset.</p>
     *
     * <p>A failure whose status is a client error keeps the span status unset, exactly like the same
     * status answered by a response. A timeout stays an error span even when the gateway reports it
     * with a client status code, because the gateway or the upstream ran out of time and the client
     * did not.</p>
     *
     * <p>The error type is recorded exactly when the request is an error span, so that it can be
     * used to count failures without counting client errors.</p>
     *
     * @param exchange the current exchange
     * @param span the gateway span of the request
     * @param failure the failure of the request, null when the request did not fail
     * @param signalType the terminal signal of the request
     */
    public static void recordOutcome(final ServerWebExchange exchange, final Span span,
                                     final Throwable failure, final SignalType signalType) {
        setAttribute(span, TracingConstants.CONTEXT_PATH, contextPath(exchange));
        setAttribute(span, TracingConstants.UPSTREAM_HOST, upstreamHost(exchange));
        setAttribute(span, TracingConstants.SELECTOR_ID, selectorId(exchange));
        final Integer failureStatus = failureStatus(failure);
        final Integer statusCode = Objects.nonNull(failureStatus) ? failureStatus : statusCode(exchange);
        if (Objects.nonNull(statusCode)) {
            span.setAttribute(TracingConstants.HTTP_RESPONSE_STATUS_CODE, (long) statusCode);
        }
        if (Objects.nonNull(failure)) {
            span.recordException(failure);
            final String errorType = errorType(failure, statusCode);
            // a client error keeps the span status unset, exactly like the same status answered by a
            // response, unless it is a timeout, which is an error of the gateway or of the upstream.
            // The type of a client error stays in the exception event of the span.
            if (isClientError(statusCode) && !TracingConstants.ERROR_TYPE_TIMEOUT.equals(errorType)) {
                return;
            }
            setAttribute(span, TracingConstants.ERROR_TYPE, errorType);
            span.setStatus(StatusCode.ERROR, failure.getClass().getSimpleName());
            return;
        }
        if (SignalType.CANCEL == signalType) {
            span.setAttribute(TracingConstants.CANCELLED, true);
            return;
        }
        if (isTimeoutStatus(statusCode)) {
            // a timeout which the gateway answered with a response, for example the fallback of a
            // circuit breaker, is the same outcome as a timeout which surfaced as an exception.
            span.setAttribute(TracingConstants.ERROR_TYPE, TracingConstants.ERROR_TYPE_TIMEOUT);
            span.setStatus(StatusCode.ERROR, "HTTP " + statusCode);
            return;
        }
        // a server error is an error span, a client error is not, as the OpenTelemetry http
        // conventions ask for.
        if (Objects.nonNull(statusCode) && statusCode >= HttpStatus.INTERNAL_SERVER_ERROR.value()) {
            span.setStatus(StatusCode.ERROR, "HTTP " + statusCode);
        }
    }

    /**
     * Acquires the route of the matched api, which is the path the client registered.
     *
     * @param exchange the current exchange
     * @return the route, null when the request did not match a registered api
     */
    private static String route(final ServerWebExchange exchange) {
        final MetaData metaData = exchange.getAttribute(Constants.META_DATA);
        return Objects.isNull(metaData) ? null : metaData.getPath();
    }

    /**
     * Acquires the context path of the request.
     *
     * <p>The context path of the http like protocols is written by the context path plugin while the
     * chain runs, so it is read once the chain has run and only then from the ShenYu context.</p>
     *
     * @param exchange the current exchange
     * @return the context path, null when the request has none
     */
    private static String contextPath(final ServerWebExchange exchange) {
        final Object contextPath = exchange.getAttributes().get(Constants.CONTEXT_PATH);
        if (contextPath instanceof String) {
            return (String) contextPath;
        }
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        return Objects.isNull(shenyuContext) ? null : shenyuContext.getContextPath();
    }

    /**
     * Acquires the host of the upstream which served the request.
     *
     * @param exchange the current exchange
     * @return the upstream host, null while the upstream is not routed yet
     */
    private static String upstreamHost(final ServerWebExchange exchange) {
        final URI upstreamUri = exchange.getAttribute(Constants.HTTP_URI);
        return Objects.isNull(upstreamUri) ? null : upstreamUri.getHost();
    }

    /**
     * Acquires the selector id of the routing decision.
     *
     * <p>Only the protocol plugins which expose the routing decision in the exchange attributes are
     * covered; the dubbo and grpc plugins keep it out of the exchange, so their spans do not carry
     * the attribute.</p>
     *
     * @param exchange the current exchange
     * @return the selector id, null when the protocol does not expose it
     */
    private static String selectorId(final ServerWebExchange exchange) {
        final Object selectorId = exchange.getAttributes().get(Constants.DIVIDE_SELECTOR_ID);
        return selectorId instanceof String ? (String) selectorId : null;
    }

    /**
     * Acquires the response status of the request.
     *
     * @param exchange the current exchange
     * @return the status code, null while the response is not written
     */
    private static Integer statusCode(final ServerWebExchange exchange) {
        final HttpStatusCode statusCode = exchange.getResponse().getStatusCode();
        return Objects.isNull(statusCode) ? null : statusCode.value();
    }

    /**
     * Acquires the status code a failure carries, for example when a refused request is answered with
     * {@code 429} or a retry gives up with {@code 408}.
     *
     * @param throwable the failure of the request
     * @return the status code of the failure, null when it does not carry one
     */
    private static Integer failureStatus(final Throwable throwable) {
        Throwable current = throwable;
        for (int depth = 0; Objects.nonNull(current) && depth < MAX_CAUSE_DEPTH; depth++) {
            if (current instanceof ResponseStatusException) {
                return ((ResponseStatusException) current).getStatusCode().value();
            }
            current = current.getCause();
        }
        return null;
    }

    /**
     * Acquires the error type of a failure, the timeout outcome or the exception type.
     *
     * @param throwable the failure of the request
     * @param statusCode the status code of the failure, null when it does not carry one
     * @return the error type
     */
    private static String errorType(final Throwable throwable, final Integer statusCode) {
        if (isTimeout(throwable) || isTimeoutStatus(statusCode)) {
            return TracingConstants.ERROR_TYPE_TIMEOUT;
        }
        return throwable.getClass().getSimpleName();
    }

    /**
     * Tells whether a status code reports a timeout.
     *
     * <p>The gateway answers a retried request with {@code 408} and wraps the timeout of an upstream
     * call into {@code 504}.</p>
     *
     * @param statusCode the status code of the failure, null when it does not carry one
     * @return true when the status reports a timeout
     */
    private static boolean isTimeoutStatus(final Integer statusCode) {
        return Objects.equals(HttpStatus.REQUEST_TIMEOUT.value(), statusCode)
                || Objects.equals(HttpStatus.GATEWAY_TIMEOUT.value(), statusCode);
    }

    /**
     * Tells whether a status code reports a client error.
     *
     * @param statusCode the status code of the failure, null when it does not carry one
     * @return true when the status is a client error
     */
    private static boolean isClientError(final Integer statusCode) {
        return Objects.nonNull(statusCode) && statusCode >= HttpStatus.BAD_REQUEST.value()
                && statusCode < HttpStatus.INTERNAL_SERVER_ERROR.value();
    }

    /**
     * Tells whether a throwable reports a timeout, including a timeout which a plugin wrapped into
     * the status exception of its own protocol.
     *
     * <p>The plugins throw the timeout exception of their own protocol, for example the http client
     * plugin throws {@code ShenyuTimeoutException} and maps a plain {@code TimeoutException} to a
     * {@code 504}, and the tracing plugin does not depend on those modules, so a timeout is
     * recognized by the type, by the name and by the cause of the failure.</p>
     *
     * @param throwable the throwable to inspect
     * @return true when the throwable reports a timeout
     */
    private static boolean isTimeout(final Throwable throwable) {
        Throwable current = throwable;
        for (int depth = 0; Objects.nonNull(current) && depth < MAX_CAUSE_DEPTH; depth++) {
            if (current instanceof TimeoutException
                    || current.getClass().getSimpleName().toLowerCase(Locale.ROOT).contains(TIMEOUT_NAME_FRAGMENT)) {
                return true;
            }
            current = current.getCause();
        }
        return false;
    }

    /**
     * Sets a span attribute unless its value is blank.
     *
     * @param span the gateway span of the request
     * @param key the attribute key
     * @param value the attribute value
     */
    private static void setAttribute(final Span span, final String key, final String value) {
        if (StringUtils.isNotBlank(value)) {
            span.setAttribute(key, value);
        }
    }
}
