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

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test Cases for PluginEnum.
 */
public final class PluginEnumTest {

    @Test
    public void testGetPluginEnumByName() {
        Arrays.stream(PluginEnum.values())
                .forEach(pluginEnum -> assertEquals(pluginEnum, PluginEnum.getPluginEnumByName(pluginEnum.getName())));
    }

    @Test
    public void testGetPluginEnumByNameInvalid() {
        assertEquals(PluginEnum.GLOBAL, PluginEnum.getPluginEnumByName("invalidName"));
    }

    @Test
    public void testGetUpstreamNames() {
        List<String> list = PluginEnum.getUpstreamNames();
        assert list.size() > 0;
    }

    @Test
    public void testGetCode() {
        Arrays.stream(PluginEnum.values())
                .forEach(pluginEnum -> assertEquals(pluginEnum.getCode(), PluginEnum.getPluginEnumByName(pluginEnum.getName()).getCode()));
    }

    @Test
    public void testGetRole() {
        Arrays.stream(PluginEnum.values())
                .forEach(pluginEnum -> assertEquals(pluginEnum.getRole(), PluginEnum.getPluginEnumByName(pluginEnum.getName()).getRole()));
    }
}
