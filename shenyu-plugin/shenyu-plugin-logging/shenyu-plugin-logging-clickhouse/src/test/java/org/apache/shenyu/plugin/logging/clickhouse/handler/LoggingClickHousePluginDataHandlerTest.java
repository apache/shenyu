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

package org.apache.shenyu.plugin.logging.clickhouse.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The Test Case For ClickHousePluginDataHandler.
 */
public final class LoggingClickHousePluginDataHandlerTest {

    private LoggingClickHousePluginDataHandler loggingClickHousePluginDataHandler;

    @BeforeEach
    public void setUp() {
        loggingClickHousePluginDataHandler = new LoggingClickHousePluginDataHandler();
    }

    @Test
    public void testHandlerPlugin() {
        PluginData pluginData = new PluginData();
        pluginData.setConfig("{\"host\":\"127.0.0.1\",\"port\":\"8123\",\"database\":\"shenyu-gateway\",\"username\":\"foo\",\"password\":\"bar\"}");
        pluginData.setEnabled(true);
        pluginData.setId("37");
        pluginData.setName("loggingClickHouse");
        pluginData.setRole("Logging");
        pluginData.setSort(195);
        loggingClickHousePluginDataHandler.handlerPlugin(pluginData);
        Assertions.assertNull(null);
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(loggingClickHousePluginDataHandler.pluginNamed(), "loggingClickHouse");
    }

}
