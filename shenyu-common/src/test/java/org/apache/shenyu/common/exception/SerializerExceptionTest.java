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

package org.apache.shenyu.common.exception;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test case for {@link SerializerException}.
 */
public final class SerializerExceptionTest {

    @Test
    public void testAcquireByThrowable() {
        String message = "error throwable";
        Throwable throwable = new Throwable(message);
        SerializerException serializerException = new SerializerException(throwable);

        assertEquals(serializerException.getCause().getMessage(), message);
        assertEquals(serializerException.getCause(), throwable);
    }

    @Test
    public void testAcquireByMessage() {
        String message = "error";
        SerializerException serializerException = new SerializerException(message);
        assertEquals(serializerException.getMessage(), message);
    }

    @Test
    public void testAcquireByMessageAndThrowable() {
        String message = "error message";
        String throwableMessage = "error throwable";
        Throwable throwable = new Throwable(throwableMessage);
        SerializerException serializerException = new SerializerException(message, throwable);
        assertEquals(serializerException.getMessage(), message);
        assertEquals(serializerException.getCause().getMessage(), throwableMessage);
        assertEquals(serializerException.getCause(), throwable);
    }
}
