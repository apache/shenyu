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

package org.apache.shenyu.common.enums;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class HttpRetryBackoffSpecEnumTest {

    @Test
    public void testAcquireByName() {
        assertEquals(HttpRetryBackoffSpecEnum.DEFAULT_BACKOFF, HttpRetryBackoffSpecEnum.acquireByName(HttpRetryBackoffSpecEnum.DEFAULT_BACKOFF.getName()));
        assertEquals(HttpRetryBackoffSpecEnum.FIXED_BACKOFF, HttpRetryBackoffSpecEnum.acquireByName(HttpRetryBackoffSpecEnum.FIXED_BACKOFF.getName()));
        assertEquals(HttpRetryBackoffSpecEnum.EXPONENTIAL_BACKOFF, HttpRetryBackoffSpecEnum.acquireByName(HttpRetryBackoffSpecEnum.EXPONENTIAL_BACKOFF.getName()));
    }

    @Test
    public void testAcquireByNameNull() {
        assertEquals(HttpRetryBackoffSpecEnum.DEFAULT_BACKOFF, HttpRetryBackoffSpecEnum.acquireByName(null));
    }

    @Test
    public void testAcquireByNameEmpty() {
        assertEquals(HttpRetryBackoffSpecEnum.DEFAULT_BACKOFF, HttpRetryBackoffSpecEnum.acquireByName(""));
    }

    @Test
    public void testAcquireByNameUnknown() {
        assertEquals(HttpRetryBackoffSpecEnum.DEFAULT_BACKOFF, HttpRetryBackoffSpecEnum.acquireByName("notExist"));
    }

    @Test
    public void testGetDefault() {
        assertEquals("default", HttpRetryBackoffSpecEnum.getDefault());
    }
}
