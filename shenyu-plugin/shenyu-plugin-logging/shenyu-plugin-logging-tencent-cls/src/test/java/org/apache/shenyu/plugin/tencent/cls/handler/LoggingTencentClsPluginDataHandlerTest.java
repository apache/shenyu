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

package org.apache.shenyu.plugin.tencent.cls.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.tencent.cls.client.TencentClsLogCollectClient;
import org.apache.shenyu.plugin.tencent.cls.config.TencentLogCollectConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * The Test Case For LoggingTencentClsPluginDataHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class LoggingTencentClsPluginDataHandlerTest {

    private LoggingTencentClsPluginDataHandler loggingTencentClsPluginDataHandler;

    private final PluginData pluginData = new PluginData();

    @BeforeEach
    public void setUp() {
        this.loggingTencentClsPluginDataHandler = Mockito.spy(new LoggingTencentClsPluginDataHandler());
        pluginData.setEnabled(true);
        pluginData.setId(UUID.randomUUID().toString().replace("-", ""));
        pluginData.setConfig("{\"topic\":\"shenyu-topic-test\", \"secretId\":\"test\", \"secretKey\":\"test\", "
                + "\"endpoint\":\"ap-guangzhou.cls.tencentcs.com\"}");
    }

    @Test
    public void testHandlerPlugin() throws NoSuchFieldException, IllegalAccessException {
        loggingTencentClsPluginDataHandler.handlerPlugin(pluginData);
        Field field = loggingTencentClsPluginDataHandler.getClass().getDeclaredField("TENCENT_CLS_LOG_COLLECT_CLIENT");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(loggingTencentClsPluginDataHandler).getClass(), TencentClsLogCollectClient.class);
        pluginData.setEnabled(false);
        loggingTencentClsPluginDataHandler.handlerPlugin(pluginData);
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(loggingTencentClsPluginDataHandler.pluginNamed(), PluginEnum.LOGGING_TENCENT_CLS.getName());
    }

    @Test
    public void testGetAliyunSlsLogCollectClient() {
        Assertions.assertEquals(LoggingTencentClsPluginDataHandler.getTencentClsLogCollectClient().getClass(), TencentClsLogCollectClient.class);
    }

    @Test
    public void testHandlerPluginUpdateSameConfig() {
        TencentLogCollectConfig.TencentClsLogConfig existingConfig = createValidConfig();
        Singleton.INST.single(existingConfig.getClass(), existingConfig);

        PluginData pluginData = createPluginData();
        pluginData.setConfig(GsonUtils.getGson().toJson(existingConfig));

        loggingTencentClsPluginDataHandler.handlerPlugin(pluginData);

        verify(loggingTencentClsPluginDataHandler, never()).doRefreshConfig(any());
    }

    @Test
    public void testHandlerPluginUpdateDifferentConfig() {
        TencentLogCollectConfig.TencentClsLogConfig existingConfig = createValidConfig();
        Singleton.INST.single(existingConfig.getClass(), existingConfig);

        TencentLogCollectConfig.TencentClsLogConfig updatedConfig = createValidConfig();
        updatedConfig.setSampleRate("0.1");
        PluginData pluginData = createPluginData();
        pluginData.setConfig(GsonUtils.getGson().toJson(updatedConfig));

        doNothing().when(loggingTencentClsPluginDataHandler).doRefreshConfig(any());

        loggingTencentClsPluginDataHandler.handlerPlugin(pluginData);

        verify(loggingTencentClsPluginDataHandler, times(1)).doRefreshConfig(any());
    }

    private PluginData createPluginData() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setId(UUID.randomUUID().toString().replace("-", ""));
        return pluginData;
    }

    private TencentLogCollectConfig.TencentClsLogConfig createValidConfig() {
        TencentLogCollectConfig.TencentClsLogConfig config = new TencentLogCollectConfig.TencentClsLogConfig();
        config.setSecretId("test");
        config.setSecretKey("test");
        config.setEndpoint("ap-guangzhou.cls.tencentcs.com");
        config.setTopic("shenyu-topic");
        config.setSendThreadCount(1);
        config.setTotalSizeInBytes("1024");
        config.setMaxSendThreadCount("1");
        config.setMaxBlockSec("60000");
        config.setMaxBatchSize("1024");
        config.setMaxBatchCount("1024");
        config.setLingerMs("2000");
        config.setRetries("10");
        config.setMaxReservedAttempts("10");
        config.setBaseRetryBackoffMs("100");
        config.setMaxRetryBackoffMs("50000");
        config.setSampleRate("1");
        return config;
    }
}
