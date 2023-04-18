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
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test Cases for TrieMatchModeEnum.
 */
public class TrieMatchModeEnumTest {

    @Test
    public void testGetMatchMode() {
        assertEquals("antPathMatch", TrieMatchModeEnum.ANT_PATH_MATCH.getMatchMode());
        assertEquals("pathPattern", TrieMatchModeEnum.PATH_PATTERN.getMatchMode());
    }

    @Test
    public void testAcquireTrieMatch() {
        assertEquals(TrieMatchModeEnum.ANT_PATH_MATCH, TrieMatchModeEnum.acquireTrieMatch("antPathMatch"));
        assertEquals(TrieMatchModeEnum.PATH_PATTERN, TrieMatchModeEnum.acquireTrieMatch("pathPattern"));
    }

    @Test
    public void testAcquireTrieMatchException() {
        assertThrows(IllegalArgumentException.class, () -> TrieMatchModeEnum.acquireTrieMatch("abc"));
    }
}
