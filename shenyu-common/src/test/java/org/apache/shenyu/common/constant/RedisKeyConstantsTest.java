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

package org.apache.shenyu.common.constant;

import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;

/**
 * Test cases for RedisKeyConstants.
 */
public final class RedisKeyConstantsTest {

    public static final String PLUGIN_INFO = ":info";

    public static final String PLUGIN_SELECTOR = ":selector";

    @Test
    public void testPlugInfoKey() {
        String mockPlugin = "MockPlugin";
        String mokPluginInfoKey = RedisKeyConstants.pluginInfoKey(mockPlugin);
        assertThat(mockPlugin, notNullValue());
        assertThat(String.join("", mockPlugin, PLUGIN_INFO), equalTo(mokPluginInfoKey));
    }

    @Test
    public void testPluginSelectorKey() {
        String mockPlugin = "MockPlugin";
        String mockPluginSelectorKey = RedisKeyConstants.pluginSelectorKey(mockPlugin);
        assertThat(mockPlugin, notNullValue());
        assertThat(String.join("", mockPlugin, PLUGIN_SELECTOR), equalTo(mockPluginSelectorKey));
    }

}
