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
import org.apache.shenyu.plugin.tencent.cls.client.TencentClsLogCollectClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

/**
 * The Test Case For LoggingTencentClsPluginDataHandler.
 */
public class LoggingTencentClsPluginDataHandlerTest {

    private LoggingTencentClsPluginDataHandler loggingTencentClsPluginDataHandler;

    private final PluginData pluginData = new PluginData();

    @BeforeEach
    public void setUp() {
        this.loggingTencentClsPluginDataHandler = new LoggingTencentClsPluginDataHandler();
        pluginData.setEnabled(true);
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
}
