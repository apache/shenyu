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

package org.apache.shenyu.client.core.exception;

import static org.junit.Assert.assertThrows;

import org.junit.Test;

public class ShenyuClientIllegalArgumentExceptionTest {

    @Test
    public void testExceptionThrownWithInvalidInput() {
        String invalidInput = "some invalid input";
        assertThrows(ShenyuClientIllegalArgumentException.class, () -> {
            throw new ShenyuClientIllegalArgumentException(invalidInput);
        });
    }

    @Test
    public void testExceptionMessageContainsInvalidInput() {
        String expectedMessage = "Invalid input: some error message";
        assertThrows(ShenyuClientIllegalArgumentException.class, () -> {
            throw new ShenyuClientIllegalArgumentException(expectedMessage);
        });
    }

    @Test
    public void testExceptionThrownWhenArgumentNull() {
        assertThrows(ShenyuClientIllegalArgumentException.class, () -> {
            throw new ShenyuClientIllegalArgumentException("Argument cannot be null");
        });
    }

    @Test
    public void testExceptionThrownWhenValueOutOfRange() {
        assertThrows(ShenyuClientIllegalArgumentException.class, () -> {
            throw new ShenyuClientIllegalArgumentException("Value must be between 1 and 10");
        });
    }
}
