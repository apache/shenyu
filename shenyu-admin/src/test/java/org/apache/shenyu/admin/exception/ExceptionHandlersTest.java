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

package org.apache.shenyu.admin.exception;

import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shiro.authz.UnauthorizedException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.MethodParameter;
import org.springframework.core.annotation.SynthesizingMethodParameter;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ExceptionHandlers}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ExceptionHandlersTest {

    private static Logger loggerSpy;

    private static MockedStatic<LoggerFactory> loggerFactoryMockedStatic;

    private ExceptionHandlers exceptionHandlersUnderTest;

    @BeforeAll
    public static void beforeClass() {
        loggerSpy = spy(LoggerFactory.getLogger(ExceptionHandlers.class));
        loggerFactoryMockedStatic = mockStatic(LoggerFactory.class);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(ExceptionHandlers.class)).thenReturn(loggerSpy);
    }

    @AfterAll
    public static void afterClass() {
        loggerFactoryMockedStatic.close();
    }

    @BeforeEach
    public void setUp() {
        exceptionHandlersUnderTest = new ExceptionHandlers();
    }

    @Test
    public void testServerExceptionHandlerByException() {
        Exception exception = new Exception();
        ShenyuAdminResult result = exceptionHandlersUnderTest.handleExceptionHandler(exception);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
        assertEquals(result.getMessage(), "The system is busy, please try again later");
    }

    @Test
    public void testServerExceptionHandlerByShenyuException() {
        Exception shenyuException = new ShenyuException("Test shenyuException message!");
        ShenyuAdminResult result = exceptionHandlersUnderTest.handleExceptionHandler(shenyuException);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
        assertEquals(result.getMessage(), shenyuException.getMessage());
    }

    @Test
    public void testServerExceptionHandlerByDuplicateKeyException() {
        DuplicateKeyException duplicateKeyException = new DuplicateKeyException("Test duplicateKeyException message!");
        ShenyuAdminResult result = exceptionHandlersUnderTest.handleDuplicateKeyException(duplicateKeyException);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
        assertEquals(result.getMessage(), ShenyuResultMessage.UNIQUE_INDEX_CONFLICT_ERROR);
    }

    @Test
    public void testShiroExceptionHandler() {
        UnauthorizedException unauthorizedException = new UnauthorizedException("Test unauthorizedException");
        ShenyuAdminResult result = exceptionHandlersUnderTest.handleUnauthorizedException(unauthorizedException);
        assertEquals(result.getCode().intValue(), CommonErrorCode.TOKEN_NO_PERMISSION);
        assertEquals(result.getMessage(), ShenyuResultMessage.TOKEN_HAS_NO_PERMISSION);
    }

    @Test
    public void testNullPointExceptionHandler() {
        NullPointerException nullPointerException = new NullPointerException("TEST NULL POINT EXCEPTION");
        ShenyuAdminResult result = exceptionHandlersUnderTest.handleNullPointException(nullPointerException);
        assertEquals(result.getCode().intValue(), CommonErrorCode.NOT_FOUND_EXCEPTION);
        assertEquals(result.getMessage(), ShenyuResultMessage.NOT_FOUND_EXCEPTION);
    }

    @Test
    public void testHandleHttpRequestMethodNotSupportedException() {
        String[] supportedMethod = new String[]{"POST", "GET"};
        HttpRequestMethodNotSupportedException exception = new HttpRequestMethodNotSupportedException("POST", supportedMethod, "request method");
        ShenyuAdminResult result = exceptionHandlersUnderTest.handleHttpRequestMethodNotSupportedException(exception);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
        assertThat(result.getMessage(), containsString("method is not supported for this request. Supported methods are"));
    }

    @Test
    public void testHandleMethodArgumentNotValidException() throws InstantiationException, IllegalAccessException, NoSuchMethodException {
        BindingResult bindingResult = spy(new DirectFieldBindingResult("test", "TestClass"));
        MethodParameter methodParameter = spy(new SynthesizingMethodParameter(this.getClass().getMethod("setUp"), -1));
        MethodArgumentNotValidException exception = spy(new MethodArgumentNotValidException(methodParameter, bindingResult));

        when(exception.getBindingResult()).thenReturn(bindingResult);
        List<FieldError> fieldErrors = spy(Collections.emptyList());
        when(bindingResult.getFieldErrors()).thenReturn(fieldErrors);
        ShenyuAdminResult result = exceptionHandlersUnderTest.handleMethodArgumentNotValidException(exception);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
        assertThat(result.getMessage(), containsString("Request error! invalid argument"));
    }

    @Test
    public void testHandleMissingServletRequestParameterException() {
        MissingServletRequestParameterException exception = spy(mock(MissingServletRequestParameterException.class));
        ShenyuAdminResult result = exceptionHandlersUnderTest.handleMissingServletRequestParameterException(exception);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
        assertThat(result.getMessage(), containsString("parameter is missing"));
    }

    @Test
    public void testHandleMethodArgumentTypeMismatchException() {
        MethodArgumentTypeMismatchException exception = spy(mock(MethodArgumentTypeMismatchException.class));
        Class clazz = MethodArgumentTypeMismatchException.class;
        when(exception.getRequiredType()).thenReturn(clazz);
        ShenyuAdminResult result = exceptionHandlersUnderTest.handleMethodArgumentTypeMismatchException(exception);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
        assertThat(result.getMessage(), containsString("should be of type"));
    }

    @Test
    public void testHandleConstraintViolationException() {
        ConstraintViolationException exception = spy(new ConstraintViolationException(Collections.emptySet()));
        Set<ConstraintViolation<?>> violations = spy(Collections.emptySet());
        when(exception.getConstraintViolations()).thenReturn(violations);
        ShenyuAdminResult result = exceptionHandlersUnderTest.handleConstraintViolationException(exception);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
    }
}
