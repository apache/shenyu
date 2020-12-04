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

package org.dromara.soul.common.enums;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

/**
 * Test Cases for DataEventTypeEnum.
 *
 * @author yanbing
 */
public final class DataEventTypeEnumTest {

    @Test
    public void testGetDataEventTypeByName() {
        DataEventTypeEnum deleteEnum = DataEventTypeEnum.DELETE;
        DataEventTypeEnum deleteEventTypeEnum = DataEventTypeEnum.acquireByName(deleteEnum.name());
        assertEquals(deleteEnum, deleteEventTypeEnum);

        DataEventTypeEnum createEnum = DataEventTypeEnum.CREATE;
        DataEventTypeEnum createEventTypeEnum = DataEventTypeEnum.acquireByName(createEnum.name());
        assertEquals(createEnum, createEventTypeEnum);
        DataEventTypeEnum updateEnum = DataEventTypeEnum.UPDATE;
        DataEventTypeEnum updateEventTypeEnum = DataEventTypeEnum.acquireByName(updateEnum.name());
        assertEquals(updateEnum, updateEventTypeEnum);

        DataEventTypeEnum myselfEnum = DataEventTypeEnum.MYSELF;
        DataEventTypeEnum myselfEventTypeEnum = DataEventTypeEnum.acquireByName(myselfEnum.name());
        assertEquals(myselfEnum, myselfEventTypeEnum);

        DataEventTypeEnum refreshEnum = DataEventTypeEnum.REFRESH;
        DataEventTypeEnum refreshEventTypeEnum = DataEventTypeEnum.acquireByName(refreshEnum.name());
        assertEquals(refreshEnum, refreshEventTypeEnum);

        assertNotEquals(refreshEnum, myselfEventTypeEnum);
        assertNotEquals(refreshEnum, updateEventTypeEnum);

    }
}
