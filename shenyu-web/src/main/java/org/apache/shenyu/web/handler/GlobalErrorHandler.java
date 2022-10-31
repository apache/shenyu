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

import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.reactive.error.ErrorWebExceptionHandler;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.lang.NonNull;
import org.springframework.util.StringUtils;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * GlobalErrorHandler.
 */
public class GlobalErrorHandler implements ErrorWebExceptionHandler {
    
    /**
     * logger.
     */
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
        LOG.error("handle error: {}{}", exchange.getLogPrefix(), formatError(throwable, exchange.getRequest()), throwable);
        HttpStatus httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
        String errMsg = httpStatus.getReasonPhrase();
        if (throwable instanceof ResponseStatusException) {
            httpStatus = ((ResponseStatusException) throwable).getStatus();
            if (StringUtils.hasLength(((ResponseStatusException) throwable).getReason())) {
                errMsg = ((ResponseStatusException) throwable).getReason();
            }
        }
        exchange.getResponse().setStatusCode(httpStatus);
        Object error = ShenyuResultWrap.error(exchange, httpStatus.value(), errMsg, throwable);
        return WebFluxResultUtils.result(exchange, error);
    }
    
    /**
     * log error info.
     *
     * @param throwable the throwable
     * @param request   the request
     */
    private String formatError(final Throwable throwable, final ServerHttpRequest request) {
        String reason = throwable.getClass().getSimpleName() + ": " + throwable.getMessage();
        return "Resolved [" + reason + "] for HTTP " + request.getMethod() + " " + request.getPath();
    }
}


