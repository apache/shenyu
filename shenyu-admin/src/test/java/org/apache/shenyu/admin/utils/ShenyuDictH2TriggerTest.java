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

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ShenyuDictH2Trigger}.
 */
public final class ShenyuDictH2TriggerTest {

    @Test
    public void testShenyuDictH2Trigger() throws SQLException {
        final ShenyuDictH2Trigger shenyuDictH2Trigger = new ShenyuDictH2Trigger();
        final Connection connection = mock(Connection.class);
        when(connection.prepareStatement(anyString())).thenReturn(mock(PreparedStatement.class));
        Assertions.assertDoesNotThrow(() -> shenyuDictH2Trigger.fire(connection, new Object[1], new Object[1]));
        Assertions.assertDoesNotThrow(shenyuDictH2Trigger::close);
        Assertions.assertDoesNotThrow(shenyuDictH2Trigger::remove);
        Assertions.assertDoesNotThrow(() -> shenyuDictH2Trigger.init(null, null, null, null, true, 0));
    }
}
