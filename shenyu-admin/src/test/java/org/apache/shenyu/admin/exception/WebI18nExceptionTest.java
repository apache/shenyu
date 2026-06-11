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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.i18n.LocaleContextHolder;

import java.util.Locale;

import org.junit.jupiter.api.Assertions;

/**
 * Test case for {@link WebI18nException}.
 */
@ExtendWith(MockitoExtension.class)
public class WebI18nExceptionTest {

    @Test
    void testConstructorWithThrowable() {
        Throwable cause = new RuntimeException("test cause");
        WebI18nException exception = new WebI18nException(cause);

        Assertions.assertSame(cause, exception.getCause());
        Assertions.assertEquals(Locale.getDefault(), exception.getLocale());
        Assertions.assertNull(exception.getArgs());
    }

    @Test
    void testConstructorWithMessage() {
        Locale locale = Locale.US;
        String message = "Test message";

        try (MockedStatic<LocaleContextHolder> localeContextHolderMock = org.mockito.Mockito.mockStatic(LocaleContextHolder.class)) {
            localeContextHolderMock.when(LocaleContextHolder::getLocale).thenReturn(locale);

            WebI18nException exception = new WebI18nException(message);

            Assertions.assertEquals(message, exception.getMessage());
            Assertions.assertEquals(locale, exception.getLocale());
            Assertions.assertInstanceOf(Object[].class, exception.getArgs());
        }
    }

    @Test
    void testConstructorWithMessageAndArgs() {
        Locale locale = Locale.FRANCE;
        String message = "Test message with args";
        Object[] args = new Object[]{"arg1", "arg2"};

        try (MockedStatic<LocaleContextHolder> localeContextHolderMock = org.mockito.Mockito.mockStatic(LocaleContextHolder.class)) {
            localeContextHolderMock.when(LocaleContextHolder::getLocale).thenReturn(locale);

            WebI18nException exception = new WebI18nException(message, args);

            Assertions.assertEquals(message, exception.getMessage());
            Assertions.assertEquals(locale, exception.getLocale());
            Assertions.assertArrayEquals(args, exception.getArgs());
        }
    }

    @Test
    void testConstructorWithMessageAndThrowable() {
        Locale locale = Locale.CHINA;
        String message = "Test message with cause";
        Throwable cause = new IllegalArgumentException("test cause");

        try (MockedStatic<LocaleContextHolder> localeContextHolderMock = org.mockito.Mockito.mockStatic(LocaleContextHolder.class)) {
            localeContextHolderMock.when(LocaleContextHolder::getLocale).thenReturn(locale);

            WebI18nException exception = new WebI18nException(message, cause);

            Assertions.assertEquals(message, exception.getMessage());
            Assertions.assertSame(cause, exception.getCause());
            Assertions.assertEquals(locale, exception.getLocale());
            Assertions.assertInstanceOf(Object[].class, exception.getArgs());
        }
    }
}
