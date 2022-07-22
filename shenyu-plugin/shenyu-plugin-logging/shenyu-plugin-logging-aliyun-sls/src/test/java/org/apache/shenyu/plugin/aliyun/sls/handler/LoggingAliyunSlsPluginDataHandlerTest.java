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
import org.apache.shenyu.plugin.aliyun.sls.client.AliyunSlsLogCollectClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

/**
 * The Test Case For LoggingAliYunSlsPluginDataHandler.
 */
public class LoggingAliyunSlsPluginDataHandlerTest {

    private LoggingAliyunSlsPluginDataHandler loggingAliYunSlsPluginDataHandler;

    private final PluginData pluginData = new PluginData();

    @BeforeEach
    public void setUp() {
        this.loggingAliYunSlsPluginDataHandler = new LoggingAliyunSlsPluginDataHandler();
        pluginData.setEnabled(true);
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
}
