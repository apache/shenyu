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

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Test case for {@link ShenyuException}.
 */
public final class ShenyuExceptionTest {

    @Test
    public void testAcquireByThrowable() {
        String message = "error throwable";
        Throwable throwable = new Throwable(message);
        ShenyuException shenyuException = new ShenyuException(throwable);

        assertTrue(shenyuException instanceof ShenyuException);
        assertEquals(shenyuException.getCause().getMessage(), message);
        assertEquals(shenyuException.getCause(), throwable);
    }

    @Test
    public void testAcquireByMessage() {
        String message = "error";
        ShenyuException shenyuException = new ShenyuException(message);

        assertTrue(shenyuException instanceof ShenyuException);
        assertEquals(shenyuException.getMessage(), message);
    }

    @Test
    public void testAcquireByMessageAndThrowable() {
        String message = "error message";
        String throwableMessage = "error throwable";
        Throwable throwable = new Throwable(throwableMessage);
        ShenyuException shenyuException = new ShenyuException(message, throwable);

        assertTrue(shenyuException instanceof ShenyuException);
        assertEquals(shenyuException.getMessage(), message);
        assertEquals(shenyuException.getCause().getMessage(), throwableMessage);
        assertEquals(shenyuException.getCause(), throwable);
    }
}
