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

/**
 * Test Cases for PluginRoleEnum.
 */
public final class PluginRoleEnumTest {

    @Test
    public void testGetCode() {
        assertEquals(0, PluginRoleEnum.SYS.getCode().intValue());
        assertEquals(1, PluginRoleEnum.CUSTOM.getCode().intValue());
    }

    @Test
    public void testGetName() {
        assertEquals("sys", PluginRoleEnum.SYS.getName());
        assertEquals("custom", PluginRoleEnum.CUSTOM.getName());
    }
}
