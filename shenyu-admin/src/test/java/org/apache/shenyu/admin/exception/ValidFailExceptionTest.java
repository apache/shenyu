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

import org.junit.jupiter.api.Assertions;

/**
 * Test case for {@link ValidFailException}.
 */
public class ValidFailExceptionTest {

    @Test
    void testConstructorWithThrowable() {
        Throwable cause = new RuntimeException("test cause");
        ValidFailException exception = new ValidFailException(cause);

        Assertions.assertSame(cause, exception.getCause());
        Assertions.assertThrows(RuntimeException.class, () -> {
            throw exception;
        });
    }

    @Test
    void testConstructorWithMessage() {
        String message = "Validation failed";
        ValidFailException exception = new ValidFailException(message);

        Assertions.assertEquals(message, exception.getMessage());
        Assertions.assertNull(exception.getCause());
    }

    @Test
    void testConstructorWithMessageAndThrowable() {
        String message = "Validation failed with cause";
        Throwable cause = new IllegalArgumentException("test cause");
        ValidFailException exception = new ValidFailException(message, cause);

        Assertions.assertEquals(message, exception.getMessage());
        Assertions.assertSame(cause, exception.getCause());
    }
}
