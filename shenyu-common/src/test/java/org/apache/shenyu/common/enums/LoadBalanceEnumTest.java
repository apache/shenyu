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

/**
 * Test case for LoadBalanceEnum.
 */
public final class LoadBalanceEnumTest {

    @Test
    public void testHashLoadBalance() {
        assertEquals(1, LoadBalanceEnum.HASH.getCode());
        assertEquals("hash", LoadBalanceEnum.HASH.getName());
        assertTrue(LoadBalanceEnum.HASH.isSupport());
    }

    @Test
    public void testRandomLoadBalance() {
        assertEquals(2, LoadBalanceEnum.RANDOM.getCode());
        assertEquals("random", LoadBalanceEnum.RANDOM.getName());
        assertTrue(LoadBalanceEnum.RANDOM.isSupport());
    }

    @Test
    public void testRoundRobinLoadBalance() {
        assertEquals(3, LoadBalanceEnum.ROUND_ROBIN.getCode());
        assertEquals("roundRobin", LoadBalanceEnum.ROUND_ROBIN.getName());
        assertTrue(LoadBalanceEnum.ROUND_ROBIN.isSupport());
    }
}
