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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.boot.web.reactive.error.ErrorWebExceptionHandler;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.filter.reactive.HiddenHttpMethodFilter;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;

/**
 * Test case for {@link ErrorHandlerConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class ErrorHandlerConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ErrorHandlerConfiguration.class))
            .withBean(ErrorHandlerConfigurationTest.class)
            .withPropertyValues("debug=true");
    }

    @Test
    public void testErrorWebExceptionHandler() {
        applicationContextRunner.run(context -> {
            ErrorWebExceptionHandler globalErrorHandler = context.getBean("errorWebExceptionHandler", ErrorWebExceptionHandler.class);
            assertNotNull(globalErrorHandler);
        });
    }

    @Test
    public void testHiddenHttpMethodFilter() {
        applicationContextRunner.run(context -> {
            HiddenHttpMethodFilter hiddenHttpMethodFilter = context.getBean("hiddenHttpMethodFilter", HiddenHttpMethodFilter.class);
            hiddenHttpMethodFilter.filter(mock(ServerWebExchange.class), mock(WebFilterChain.class));
            assertNotNull(hiddenHttpMethodFilter);
        });
    }

}
