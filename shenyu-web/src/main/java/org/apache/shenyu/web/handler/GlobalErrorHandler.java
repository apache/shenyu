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

package org.apache.shenyu.web.handler;

import org.apache.shenyu.plugin.api.failure.GatewayFailure;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.alert.AlarmSender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.reactive.error.ErrorWebExceptionHandler;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.lang.NonNull;
import org.springframework.util.StringUtils;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CancellationException;
import java.util.concurrent.TimeoutException;

/**
 * GlobalErrorHandler.
 */
public class GlobalErrorHandler implements ErrorWebExceptionHandler {

    private static final Logger LOG = LoggerFactory.getLogger(GlobalErrorHandler.class);

    /**
     * handler error.
     *
     * @param exchange  the exchange
     * @param throwable the throwable
     * @return error result
     */
    @Override
    @NonNull
    public Mono<Void> handle(@NonNull final ServerWebExchange exchange, @NonNull final Throwable throwable) {
        final GatewayFailure facts = WebFluxResultUtils.failure(exchange, classify(throwable), GatewayFailure.Stage.GLOBAL_HANDLER, null);
        if (facts.reason() == GatewayFailure.Reason.CANCELLATION || facts.output() == GatewayFailure.Output.STARTED_OR_COMMITTED) {
            return WebFluxResultUtils.failureResult(exchange, facts, null, 500, "Internal Server Error", throwable);
        }
        LOG.error("handle error: {} formatError:{} throwable:", exchange.getLogPrefix(), formatError(throwable, exchange.getRequest()), throwable);
        HttpStatusCode httpStatusCode;
        String errorMsg = "";
        if (throwable instanceof IllegalArgumentException) {
            httpStatusCode = HttpStatus.BAD_REQUEST;
            errorMsg = throwable.getMessage();
        } else if (throwable instanceof ResponseStatusException) {
            ResponseStatusException responseStatusException = (ResponseStatusException) throwable;
            httpStatusCode = responseStatusException.getStatusCode();
            String errMsg = responseStatusException.getReason();
            if (!StringUtils.hasLength(errMsg)) {
                HttpStatus httpStatus = HttpStatus.resolve(httpStatusCode.value());
                errMsg = Objects.isNull(httpStatus) ? httpStatusCode.toString() : httpStatus.getReasonPhrase();
            }
            errorMsg = errMsg;
        } else {
            httpStatusCode = HttpStatus.INTERNAL_SERVER_ERROR;
            HttpStatus httpStatus = (HttpStatus) httpStatusCode;
            errorMsg = httpStatus.getReasonPhrase();
        }
        Map<String, String> labels = new HashMap<>(8);
        labels.put("global", "error");
        labels.put("component", "gateway");
        AlarmSender.alarmMediumCritical("ShenYu-Gateway-Global-Error", errorMsg, labels);
        return WebFluxResultUtils.failureResult(exchange, facts, httpStatusCode, httpStatusCode.value(), errorMsg, throwable);
    }

    private GatewayFailure.Reason classify(final Throwable error) {
        Throwable current = error;
        for (int depth = 0; depth < 16 && Objects.nonNull(current); depth++) {
            if (current instanceof CancellationException) {
                return GatewayFailure.Reason.CANCELLATION;
            }
            if (current instanceof TimeoutException) {
                return GatewayFailure.Reason.DEADLINE;
            }
            current = current.getCause();
        }
        if (error instanceof ResponseStatusException status) {
            return switch (status.getStatusCode().value()) {
                case 401 -> GatewayFailure.Reason.AUTHENTICATION;
                case 403 -> GatewayFailure.Reason.AUTHORIZATION;
                case 429 -> GatewayFailure.Reason.RESOURCE_LIMIT;
                case 504 -> GatewayFailure.Reason.DEADLINE;
                case 502, 503 -> GatewayFailure.Reason.UPSTREAM;
                default -> GatewayFailure.Reason.UNKNOWN;
            };
        }
        return error instanceof IllegalArgumentException ? GatewayFailure.Reason.INVALID_REQUEST : GatewayFailure.Reason.UNKNOWN;
    }

    /**
     * log error info.
     *
     * @param throwable the throwable
     * @param request   the request
     */
    private String formatError(final Throwable throwable, final ServerHttpRequest request) {
        String reason = throwable.getClass().getSimpleName() + ": " + throwable.getMessage();
        return "Resolved [" + reason + "] for HTTP " + request.getMethod() + " " + request.getURI().getRawPath();
    }
}
