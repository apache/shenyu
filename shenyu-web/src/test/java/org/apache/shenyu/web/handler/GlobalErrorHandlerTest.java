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

import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.web.ErrorProperties;
import org.springframework.boot.autoconfigure.web.ResourceProperties;
import org.springframework.boot.web.reactive.context.AnnotationConfigReactiveWebApplicationContext;
import org.springframework.boot.web.reactive.error.DefaultErrorAttributes;
import org.springframework.boot.web.reactive.error.ErrorAttributes;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.reactive.function.server.MockServerRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.ServerResponse;
import org.springframework.web.reactive.result.view.ViewResolver;
import org.springframework.web.server.ServerWebExchange;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * test for GlobalErrorHandler.
 *
 */
@RunWith(MockitoJUnitRunner.class)
public class GlobalErrorHandlerTest {

    private static Logger loggerSpy;

    private static MockedStatic<LoggerFactory> loggerFactoryMockedStatic;

    private GlobalErrorHandler globalErrorHandler;

    @BeforeClass
    public static void beforeClass() {
        loggerSpy = spy(LoggerFactory.getLogger(GlobalErrorHandler.class));
        loggerFactoryMockedStatic = mockStatic(LoggerFactory.class);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(GlobalErrorHandler.class)).thenReturn(loggerSpy);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(anyString())).thenReturn(loggerSpy);
    }

    @AfterClass
    public static void afterClass() {
        loggerFactoryMockedStatic.close();
    }

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult() { });

        ErrorAttributes errorAttributes = new DefaultErrorAttributes();
        ResourceProperties resourceProperties = mock(ResourceProperties.class);
        ErrorProperties serverProperties = mock(ErrorProperties.class);
        ApplicationContext applicationContext = new AnnotationConfigReactiveWebApplicationContext();
        ViewResolver viewResolver = mock(ViewResolver.class);

        globalErrorHandler = new GlobalErrorHandler(errorAttributes, resourceProperties, serverProperties, applicationContext);
        globalErrorHandler.setViewResolvers(Collections.singletonList(viewResolver));
    }

    @Test
    public void getErrorAttributes() {
        doNothing().when(loggerSpy).error(anyString());
        ServerWebExchange webExchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost:8080/favicon.ico"));
        NullPointerException nullPointerException = new NullPointerException("nullPointerException");
        MockServerRequest serverRequest = MockServerRequest.builder()
                .exchange(webExchange)
                .attribute("org.springframework.boot.web.reactive.error.DefaultErrorAttributes.ERROR", nullPointerException)
                .build();
        Map<String, Object> response = globalErrorHandler.getErrorAttributes(serverRequest, false);
        assertNotNull(response);
        assertThat(response, hasEntry("code", 500));
        assertThat(response, hasEntry("message", HttpStatus.INTERNAL_SERVER_ERROR.getReasonPhrase()));
        assertThat(response, hasEntry("data", nullPointerException.getMessage()));
    }

    @Test
    public void getRoutingFunction() {
        final ErrorAttributes errorAttributes = mock(DefaultErrorAttributes.class);
        RouterFunction<ServerResponse> routerFunction = globalErrorHandler.getRoutingFunction(errorAttributes);
        assertNotNull(routerFunction);
    }

    @Test
    public void getHttpStatus() {
        final Map<String, Object> errorAttributes = new LinkedHashMap<>();
        int status = globalErrorHandler.getHttpStatus(errorAttributes);
        assertThat(status, is(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    }
}
