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

package org.apache.shenyu.web.configuration;

import org.apache.shenyu.web.handler.GlobalErrorHandler;
import org.springframework.boot.web.reactive.error.ErrorWebExceptionHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.lang.NonNull;
import org.springframework.web.filter.reactive.HiddenHttpMethodFilter;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

/**
 * The type Error handler configuration.
 */
public class ErrorHandlerConfiguration {

    /**
     * Error web exception handler error web exception handler.
     *
     * @return the error web exception handler
     */
    @Bean
    @Order(Ordered.HIGHEST_PRECEDENCE + 1)
    public ErrorWebExceptionHandler errorWebExceptionHandler() {
        return new GlobalErrorHandler();
    }

    /**
     * Hidden http method filter hidden http method filter.
     *
     * @see <a href="https://github.com/spring-cloud/spring-cloud-gateway/issues/541">issues-541</a>
     * @return the hidden http method filter
     */
    @Bean
    public HiddenHttpMethodFilter hiddenHttpMethodFilter() {
        return new HiddenHttpMethodFilter() {

            @Override
            @NonNull
            public Mono<Void> filter(@NonNull final ServerWebExchange exchange, @NonNull final WebFilterChain chain) {
                return chain.filter(exchange);
            }
        };
    }
}
