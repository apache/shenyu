/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.handler;

import org.springframework.http.HttpStatus;
import org.springframework.http.codec.HttpMessageWriter;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.reactive.function.server.ServerResponse;
import org.springframework.web.reactive.result.view.ViewResolver;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebExceptionHandler;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR;

/**
 * GlobalErrorHandler.
 *
 * @author xiaoyu(Myth)
 */
@Component
public class GlobalErrorHandler implements WebExceptionHandler {
    /**
     * Handle the given exception. A completion signal through the return value
     * indicates error handling is complete while an error signal indicates the
     * exception is still not handled.
     *
     * @param exchange the current exchange
     * @param ex       the exception to handle
     * @return {@code Mono<Void>} to indicate when exception handling is complete
     */
    @Override
    public Mono<Void> handle(final ServerWebExchange exchange, final Throwable ex) {
        return handle(ex).flatMap(it -> it.writeTo(exchange,
                new HandlerStrategiesResponseContext(HandlerStrategies.withDefaults())))
                .flatMap(i -> Mono.empty());
    }


    private Mono<ServerResponse> handle(Throwable ex) {
        return createResponse(INTERNAL_SERVER_ERROR, "GENERIC_ERROR", "Unhandled exception");
    }

    private Mono<ServerResponse> createResponse(final HttpStatus httpStatus, final String code, final String mesage) {

        return ServerResponse.status(httpStatus).syncBody(mesage);
    }

    /**
     * The type Handler strategies response context.
     */
    static class HandlerStrategiesResponseContext implements ServerResponse.Context {

        private HandlerStrategies handlerStrategies;

        /**
         * Instantiates a new Handler strategies response context.
         *
         * @param handlerStrategies the handler strategies
         */
        public HandlerStrategiesResponseContext(final HandlerStrategies handlerStrategies) {
            this.handlerStrategies = handlerStrategies;
        }

        /**
         * Return the {@link HttpMessageWriter}s to be used for response body conversion.
         *
         * @return the list of message writers
         */
        @Override
        public List<HttpMessageWriter<?>> messageWriters() {
            return this.handlerStrategies.messageWriters();
        }

        /**
         * Return the  {@link ViewResolver}s to be used for view name resolution.
         *
         * @return the list of view resolvers
         */
        @Override
        public List<ViewResolver> viewResolvers() {
            return this.handlerStrategies.viewResolvers();
        }
    }

}


