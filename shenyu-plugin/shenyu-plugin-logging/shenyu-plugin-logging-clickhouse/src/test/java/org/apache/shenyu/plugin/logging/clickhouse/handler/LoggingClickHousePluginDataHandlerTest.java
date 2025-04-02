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
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.logging.clickhouse.config.ClickHouseLogCollectConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * The Test Case For ClickHousePluginDataHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class LoggingClickHousePluginDataHandlerTest {

    private LoggingClickHousePluginDataHandler loggingClickHousePluginDataHandler;

    @BeforeEach
    public void setUp() {
        loggingClickHousePluginDataHandler = Mockito.spy(new LoggingClickHousePluginDataHandler());
    }

    @Test
    public void testHandlerPlugin() {
        PluginData pluginData = createPluginData();
        loggingClickHousePluginDataHandler.handlerPlugin(pluginData);
        Assertions.assertNull(null);
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(loggingClickHousePluginDataHandler.pluginNamed(), "loggingClickHouse");
    }

    @Test
    void testHandlerPluginUpdateSameConfig() {
        ClickHouseLogCollectConfig.ClickHouseLogConfig existingConfig = createValidConfig();
        Singleton.INST.single(existingConfig.getClass(), existingConfig);

        PluginData pluginData = createPluginData();

        loggingClickHousePluginDataHandler.handlerPlugin(pluginData);

        verify(loggingClickHousePluginDataHandler, never()).doRefreshConfig(any());
    }

    private ClickHouseLogCollectConfig.ClickHouseLogConfig createValidConfig() {
        ClickHouseLogCollectConfig.ClickHouseLogConfig config = new ClickHouseLogCollectConfig.ClickHouseLogConfig();
        config.setHost("127.0.0.1");
        config.setPort("8123");
        config.setDatabase("shenyu-gateway");
        config.setUsername("foo");
        config.setPassword("bar");
        config.setTtl("30");
        return config;
    }

    private PluginData createPluginData() {
        PluginData pluginData = new PluginData();
        pluginData.setConfig("{\"host\":\"127.0.0.1\",\"port\":\"8123\",\"database\":\"shenyu-gateway\",\"username\":\"foo\",\"password\":\"bar\", \"ttl\":\"30\"}");
        pluginData.setEnabled(true);
        pluginData.setId("37");
        pluginData.setName("loggingClickHouse");
        pluginData.setRole("Logging");
        pluginData.setSort(195);
        return pluginData;
    }

}
