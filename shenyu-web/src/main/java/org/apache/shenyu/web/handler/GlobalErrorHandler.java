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

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.springframework.boot.autoconfigure.web.ErrorProperties;
import org.springframework.boot.autoconfigure.web.ResourceProperties;
import org.springframework.boot.autoconfigure.web.reactive.error.DefaultErrorWebExceptionHandler;
import org.springframework.boot.web.reactive.error.ErrorAttributes;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.web.reactive.function.server.RequestPredicates;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;

import java.util.Map;

/**
 * GlobalErrorHandler.
 */
@Slf4j
public class GlobalErrorHandler extends DefaultErrorWebExceptionHandler {

    /**
     * Instantiates a new Global error handler.
     *
     * @param errorAttributes    the error attributes
     * @param resourceProperties the resource properties
     * @param errorProperties    the error properties
     * @param applicationContext the application context
     */
    public GlobalErrorHandler(final ErrorAttributes errorAttributes,
                              final ResourceProperties resourceProperties,
                              final ErrorProperties errorProperties,
                              final ApplicationContext applicationContext) {
        super(errorAttributes, resourceProperties, errorProperties, applicationContext);
    }

    @Override
    protected Map<String, Object> getErrorAttributes(final ServerRequest request, final boolean includeStackTrace) {
        logError(request);
        return response(request);
    }

    @Override
    protected RouterFunction<ServerResponse> getRoutingFunction(final ErrorAttributes errorAttributes) {
        return RouterFunctions.route(RequestPredicates.all(), this::renderErrorResponse);
    }

    @Override
    protected int getHttpStatus(final Map<String, Object> errorAttributes) {
        return HttpStatus.INTERNAL_SERVER_ERROR.value();
    }

    private Map<String, Object> response(final ServerRequest request) {
        Throwable ex = getError(request);
        Object error = ShenyuResultWrap.error(HttpStatus.INTERNAL_SERVER_ERROR.value(), HttpStatus.INTERNAL_SERVER_ERROR.getReasonPhrase(), ex.getMessage());
        return GsonUtils.getInstance().toObjectMap(GsonUtils.getInstance().toJson(error));
    }

    private void logError(final ServerRequest request) {
        Throwable ex = getError(request);
        log.error(request.exchange().getLogPrefix() + formatError(ex, request));
    }

    private String formatError(final Throwable ex, final ServerRequest request) {
        String reason = ex.getClass().getSimpleName() + ": " + ex.getMessage();
        return "Resolved [" + reason + "] for HTTP " + request.methodName() + " " + request.path();
    }
}


