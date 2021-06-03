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
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.web.ResourceProperties;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.reactive.error.ErrorAttributes;
import org.springframework.boot.web.reactive.error.ErrorWebExceptionHandler;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.web.filter.reactive.HiddenHttpMethodFilter;
import org.springframework.web.reactive.result.view.ViewResolver;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.Collections;
import java.util.List;

/**
 * The type Error handler configuration.
 */
@EnableConfigurationProperties({ServerProperties.class, ResourceProperties.class})
public class ErrorHandlerConfiguration {

    private final ServerProperties serverProperties;

    private final ApplicationContext applicationContext;

    private final ResourceProperties resourceProperties;

    private final List<ViewResolver> viewResolvers;

    private final ServerCodecConfigurer serverCodecConfigurer;

    /**
     * Instantiates a new Error handler configuration.
     *
     * @param serverProperties      the server properties
     * @param resourceProperties    the resource properties
     * @param viewResolversProvider the view resolvers provider
     * @param serverCodecConfigurer the server codec configurer
     * @param applicationContext    the application context
     */
    public ErrorHandlerConfiguration(final ServerProperties serverProperties,
                                     final ResourceProperties resourceProperties,
                                     final ObjectProvider<List<ViewResolver>> viewResolversProvider,
                                     final ServerCodecConfigurer serverCodecConfigurer,
                                     final ApplicationContext applicationContext) {
        this.serverProperties = serverProperties;
        this.applicationContext = applicationContext;
        this.resourceProperties = resourceProperties;
        this.viewResolvers = viewResolversProvider.getIfAvailable(Collections::emptyList);
        this.serverCodecConfigurer = serverCodecConfigurer;
    }

    /**
     * Error web exception handler error web exception handler.
     *
     * @param errorAttributes the error attributes
     * @return the error web exception handler
     */
    @Bean
    @Order(Ordered.HIGHEST_PRECEDENCE + 1)
    public ErrorWebExceptionHandler errorWebExceptionHandler(final ErrorAttributes errorAttributes) {
        GlobalErrorHandler exceptionHandler = new GlobalErrorHandler(
                errorAttributes,
                this.resourceProperties,
                this.serverProperties.getError(),
                this.applicationContext);
        exceptionHandler.setViewResolvers(this.viewResolvers);
        exceptionHandler.setMessageWriters(this.serverCodecConfigurer.getWriters());
        exceptionHandler.setMessageReaders(this.serverCodecConfigurer.getReaders());
        return exceptionHandler;
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
            public Mono<Void> filter(final ServerWebExchange exchange, final WebFilterChain chain) {
                return chain.filter(exchange);
            }
        };
    }
}
