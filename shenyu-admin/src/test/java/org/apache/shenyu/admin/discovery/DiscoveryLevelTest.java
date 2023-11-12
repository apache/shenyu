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

package org.apache.shenyu.admin.discovery;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class DiscoveryLevelTest {
    @Test
    void getCode() {
        assertEquals("2", DiscoveryLevel.GLOBAL.getCode());
        assertEquals("1", DiscoveryLevel.PLUGIN.getCode());
        assertEquals("0", DiscoveryLevel.SELECTOR.getCode());
    }

    @Test
    void enumValues() {
        DiscoveryLevel[] values = DiscoveryLevel.values();
        assertEquals(3, values.length);
        assertEquals(DiscoveryLevel.GLOBAL, values[0]);
        assertEquals(DiscoveryLevel.PLUGIN, values[1]);
        assertEquals(DiscoveryLevel.SELECTOR, values[2]);
    }
}
