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

package org.apache.shenyu.plugin.aliyun.sls.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.aliyun.sls.client.AliyunSlsLogCollectClient;
import org.apache.shenyu.plugin.aliyun.sls.config.AliyunLogCollectConfig;
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
 * The Test Case For LoggingAliYunSlsPluginDataHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class LoggingAliyunSlsPluginDataHandlerTest {

    private LoggingAliyunSlsPluginDataHandler loggingAliYunSlsPluginDataHandler;

    private final PluginData pluginData = new PluginData();

    @BeforeEach
    public void setUp() {
        this.loggingAliYunSlsPluginDataHandler = Mockito.spy(new LoggingAliyunSlsPluginDataHandler());
        pluginData.setEnabled(true);
        pluginData.setId(UUID.randomUUID().toString().replace("-", ""));
        pluginData.setConfig("{\"topic\":\"shenyu-topic-test\", \"accessId\":\"test\", \"accessKey\":\"test\", "
                + "\"host\":\"cn-guangzhou.log.aliyuncs.com\", \"projectName\":\"shenyu-test\", \"logStoreName\":\"shenyu-test-logstore\"}");
    }

    @Test
    public void testHandlerPlugin() throws NoSuchFieldException, IllegalAccessException {
        loggingAliYunSlsPluginDataHandler.handlerPlugin(pluginData);
        Field field = loggingAliYunSlsPluginDataHandler.getClass().getDeclaredField("ALIYUN_SLS_LOG_COLLECT_CLIENT");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(loggingAliYunSlsPluginDataHandler).getClass(), AliyunSlsLogCollectClient.class);
        pluginData.setEnabled(false);
        loggingAliYunSlsPluginDataHandler.handlerPlugin(pluginData);
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(loggingAliYunSlsPluginDataHandler.pluginNamed(), PluginEnum.LOGGING_ALIYUN_SLS.getName());
    }

    @Test
    public void testGetAliyunSlsLogCollectClient() {
        Assertions.assertEquals(LoggingAliyunSlsPluginDataHandler.getAliyunSlsLogCollectClient().getClass(), AliyunSlsLogCollectClient.class);
    }

    @Test
    public void testHandlerPluginUpdateSameConfig() {
        AliyunLogCollectConfig.AliyunSlsLogConfig existingConfig = createValidConfig();
        Singleton.INST.single(existingConfig.getClass(), existingConfig);

        PluginData pluginData = createPluginData();
        pluginData.setConfig(GsonUtils.getGson().toJson(existingConfig));

        loggingAliYunSlsPluginDataHandler.handlerPlugin(pluginData);

        verify(loggingAliYunSlsPluginDataHandler, never()).doRefreshConfig(any());
    }

    @Test
    public void testHandlerPluginUpdateDifferentConfig() {
        AliyunLogCollectConfig.AliyunSlsLogConfig existingConfig = createValidConfig();
        Singleton.INST.single(existingConfig.getClass(), existingConfig);

        AliyunLogCollectConfig.AliyunSlsLogConfig updatedConfig = createValidConfig();
        updatedConfig.setSampleRate("1");
        updatedConfig.setBufferQueueSize(2048);
        updatedConfig.setMaxRequestBody(2048);
        updatedConfig.setMaxResponseBody(2048);
        PluginData pluginData = createPluginData();
        pluginData.setConfig(GsonUtils.getGson().toJson(updatedConfig));

        doNothing().when(loggingAliYunSlsPluginDataHandler).doRefreshConfig(any());

        loggingAliYunSlsPluginDataHandler.handlerPlugin(pluginData);

        verify(loggingAliYunSlsPluginDataHandler, times(1)).doRefreshConfig(any());
    }

    private PluginData createPluginData() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setId(UUID.randomUUID().toString().replace("-", ""));
        return pluginData;
    }

    private AliyunLogCollectConfig.AliyunSlsLogConfig createValidConfig() {
        AliyunLogCollectConfig.AliyunSlsLogConfig config = new AliyunLogCollectConfig.AliyunSlsLogConfig();
        config.setAccessId("test");
        config.setAccessKey("test");
        config.setHost("cn-guangzhou.log.aliyuncs.com");
        config.setProjectName("shenyu-test");
        config.setLogStoreName("shenyu-logstore");
        config.setTopic("shenyu-topic");
        config.setTtlInDay(3);
        config.setShardCount(10);
        config.setSendThreadCount(1);
        config.setIoThreadCount(1);
        config.setSampleRate("1");
        config.setMaxRequestBody(1024);
        config.setMaxResponseBody(1024);
        config.setBufferQueueSize(1024);
        return config;
    }
}
