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

package org.apache.shenyu.admin.utils;

import org.apache.shenyu.admin.exception.ResourceNotFoundException;
import org.apache.shenyu.admin.exception.ValidFailException;
import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.mockito.Mockito.mock;


/**
 * AssertTest.
 */
class AssertTest {

    @Test
    public void notBlack() {
        Assertions.assertDoesNotThrow(() -> Assert.notBlack("notBlack", "error message"));
        Assertions.assertThrows(ValidFailException.class, () -> Assert.notBlack("", "error message"));
    }

    @Test
    public void notEmptyTest() {
        Assertions.assertDoesNotThrow(() -> Assert.notEmpty(Collections.singleton(1), "error message"));
        Assertions.assertThrows(ValidFailException.class, () -> Assert.notEmpty(Collections.emptyList(), "error message"));
    }

    @Test
    public void throwExceptionTest() {
        Assertions.assertThrows(ResourceNotFoundException.class, () -> Assert.throwException(mock(ShenyuException.class)));
    }
}
