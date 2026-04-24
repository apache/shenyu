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

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * Test case for {@link I18nException}.
 */
public class I18nExceptionTest {

    @Test
    void testConstructorWithThrowable() {
        Throwable cause = new RuntimeException("test cause");
        I18nException exception = new I18nException(cause);

        assertSame(cause, exception.getCause());
        assertEquals(Locale.getDefault(), exception.getLocale());
        assertNull(exception.getArgs());
    }

    @Test
    void testConstructorWithLocaleMessageAndArgs() {
        Locale locale = Locale.US;
        String message = "Test message";
        Object[] args = new Object[]{"arg1", "arg2"};

        I18nException exception = new I18nException(locale, message, args);

        assertEquals(message, exception.getMessage());
        assertEquals(locale, exception.getLocale());
        assertArrayEquals(args, exception.getArgs());
    }

    @Test
    void testConstructorWithLocaleMessageThrowableAndArgs() {
        Locale locale = Locale.FRANCE;
        String message = "Test message with cause";
        Throwable cause = new IllegalArgumentException("test cause");
        Object[] args = new Object[]{"arg1"};

        I18nException exception = new I18nException(locale, message, cause, args);

        assertEquals(message, exception.getMessage());
        assertSame(cause, exception.getCause());
        assertEquals(locale, exception.getLocale());
        assertArrayEquals(args, exception.getArgs());
    }

    @Test
    void testGetLocale() {
        Locale locale = Locale.CHINA;
        I18nException exception = new I18nException(locale, "message");

        assertEquals(locale, exception.getLocale());
    }

    @Test
    void testGetArgs() {
        Object[] args = new Object[]{"test", 123};
        I18nException exception = new I18nException(Locale.getDefault(), "message", args);

        assertArrayEquals(args, exception.getArgs());
    }
}
