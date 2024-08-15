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

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;


/**
 * Test Cases for OperatorEnum.
 */
public final class OperatorEnumTest {

    /**
     * test acquireSupport method.
     */
    @Test
    public void testAcquireSupport() {
        List<OperatorEnum> enums = OperatorEnum.acquireSupport();
        assertTrue(enums.contains(OperatorEnum.MATCH));
        assertTrue(enums.contains(OperatorEnum.EQ));
        assertTrue(enums.contains(OperatorEnum.REGEX));
        assertTrue(enums.contains(OperatorEnum.CONTAINS));
        assertTrue(enums.contains(OperatorEnum.PATH_PATTERN));
        assertTrue(enums.contains(OperatorEnum.TIME_BEFORE));
        assertTrue(enums.contains(OperatorEnum.TIME_AFTER));
        assertTrue(enums.contains(OperatorEnum.STARTS_WITH));
        assertTrue(enums.contains(OperatorEnum.ENDS_WITH));
        assertTrue(enums.contains(OperatorEnum.EXCLUDE));
        assertFalse(enums.contains(OperatorEnum.GT));
        assertFalse(enums.contains(OperatorEnum.LT));
    }

    /**
     * test getOperatorEnumByAlias method.
     */
    @Test
    public void testAcquireByNameInvalid() {
        assertEquals(OperatorEnum.MATCH, OperatorEnum.getOperatorEnumByAlias("match"));
        assertEquals(OperatorEnum.EQ, OperatorEnum.getOperatorEnumByAlias("="));
        assertEquals(OperatorEnum.REGEX, OperatorEnum.getOperatorEnumByAlias("regex"));
        assertEquals(OperatorEnum.CONTAINS, OperatorEnum.getOperatorEnumByAlias("contains"));
    }

    @Test
    public void testGetSupport() {
        Arrays.stream(OperatorEnum.values())
                .forEach(operatorEnum -> assertEquals(operatorEnum.getSupport(), OperatorEnum.valueOf(operatorEnum.name()).getSupport()));
    }
}
