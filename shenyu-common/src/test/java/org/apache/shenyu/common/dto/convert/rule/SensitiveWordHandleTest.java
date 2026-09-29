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

package org.apache.shenyu.common.dto.convert.rule;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link SensitiveWordHandle}.
 */
public final class SensitiveWordHandleTest {

    @Test
    public void testDefaultInstance() {
        SensitiveWordHandle handle = SensitiveWordHandle.newDefaultInstance();
        assertEquals(SensitiveWordHandle.DEFAULT_REDIS_KEY, handle.getRedisKey());
        assertEquals(300L, handle.getRefreshIntervalSeconds());
    }

    @Test
    public void testFailClosed() {
        SensitiveWordHandle handle = new SensitiveWordHandle();
        assertFalse(handle.isFailClosed());
        handle.setFailClosed(true);
        assertTrue(handle.isFailClosed());
    }

    @Test
    public void testWordsAndMaxBodySize() {
        SensitiveWordHandle handle = new SensitiveWordHandle();
        assertNull(handle.getWords());
        assertEquals(0L, handle.getMaxBodySize());
        handle.setWords("bad, worse");
        handle.setMaxBodySize(1024L);
        assertEquals("bad, worse", handle.getWords());
        assertEquals(1024L, handle.getMaxBodySize());
    }

    @Test
    public void testDictionaryKey() {
        SensitiveWordHandle handle = new SensitiveWordHandle();
        assertEquals(SensitiveWordHandle.DEFAULT_REDIS_KEY, handle.dictionaryKey());
        handle.setRedisKey("custom:words");
        assertEquals("custom:words", handle.dictionaryKey());
        handle.setWords("bad");
        assertEquals("custom:words|bad", handle.dictionaryKey());
        // a blank word list is the same dictionary as the redis set, they must share an automaton
        handle.setWords("   ");
        assertEquals("custom:words", handle.dictionaryKey());
    }

    @Test
    public void testSetter() {
        SensitiveWordHandle handle = new SensitiveWordHandle();
        handle.setRedisKey("custom:sensitive:words");
        handle.setRefreshIntervalSeconds(30L);
        assertEquals("custom:sensitive:words", handle.getRedisKey());
        assertEquals(30L, handle.getRefreshIntervalSeconds());
    }
}
