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
import static org.junit.jupiter.api.Assertions.assertTrue;

public class RetryEnumTest {

    @Test
    public void testRetryCode() {
        assertEquals(1, RetryEnum.CURRENT.getCode());
        assertEquals(2, RetryEnum.FAILOVER.getCode());
    }

    @Test
    public void testRetryName() {
        assertEquals("current", RetryEnum.CURRENT.getName());
        assertEquals("failover", RetryEnum.FAILOVER.getName());
    }

    @Test
    public void testSupport() {
        assertTrue(RetryEnum.CURRENT.isSupport());
        assertTrue(RetryEnum.FAILOVER.isSupport());
    }
}
