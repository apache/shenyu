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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Verifies the persisted instance state contract.
 */
class InstanceStatusEnumTest {

    @Test
    void testPersistedStateCodesRemainStable() {
        assertEquals(0, InstanceStatusEnum.DELETED.getCode());
        assertEquals(1, InstanceStatusEnum.ONLINE.getCode());
        assertEquals(2, InstanceStatusEnum.OFFLINE.getCode());
        assertEquals("DELETED", InstanceStatusEnum.getNameByCode(0));
        assertEquals("ONLINE", InstanceStatusEnum.getNameByCode(1));
        assertEquals("OFFLINE", InstanceStatusEnum.getNameByCode(2));
        assertEquals(3, InstanceStatusEnum.values().length);
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, 3, 99, Integer.MIN_VALUE, Integer.MAX_VALUE})
    void testUnrecognizedStateIsUnknown(final int code) {
        assertEquals("UNKNOWN", InstanceStatusEnum.getNameByCode(code));
    }
}
