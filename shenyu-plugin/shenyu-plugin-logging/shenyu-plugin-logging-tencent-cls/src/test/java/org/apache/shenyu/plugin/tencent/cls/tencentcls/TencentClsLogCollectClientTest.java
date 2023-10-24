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

package org.apache.shenyu.plugin.tencent.cls.tencentcls;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.tencent.cls.client.TencentClsLogCollectClient;
import org.apache.shenyu.plugin.tencent.cls.config.TencentLogCollectConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

/**
 * test cases for TencentClsLogCollectClient.
 */
public class TencentClsLogCollectClientTest {

    private TencentClsLogCollectClient tencentClsLogCollectClient;
    
    private final PluginData pluginData = new PluginData();

    private TencentLogCollectConfig.TencentClsLogConfig tencentClsLogConfig;

    private final List<ShenyuRequestLog> logs = new ArrayList<>();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @BeforeEach
    public void setup() {
        this.tencentClsLogCollectClient = new TencentClsLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"topic\":\"shenyu-topic-test\", \"secretId\":\"test\", \"secretKey\":\"test\", "
                + "\"endpoint\":\"ap-guangzhou.cls.tencentcs.com\"}");
        tencentClsLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(),
                TencentLogCollectConfig.TencentClsLogConfig.class);
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        shenyuRequestLog.setRequestUri("org/apache/shenyu/plugin/logging");
        logs.add(shenyuRequestLog);
    }

    @Test
    public void testInitClient() throws NoSuchFieldException, IllegalAccessException {
        tencentClsLogCollectClient.initClient(tencentClsLogConfig);
        Field field = tencentClsLogCollectClient.getClass().getDeclaredField("topic");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(tencentClsLogCollectClient), "shenyu-topic-test");
        tencentClsLogCollectClient.close();
    }

    @Test
    public void testConsume() {
        String msg = "";
        TencentLogCollectConfig.INSTANCE.setTencentClsLogConfig(tencentClsLogConfig);
        tencentClsLogCollectClient.initClient(tencentClsLogConfig);
        try {
            tencentClsLogCollectClient.consume(logs);
        } catch (Exception e) {
            msg = "false";
        }
        Assertions.assertEquals(msg, "");
        Assertions.assertEquals(tencentClsLogConfig,
                TencentLogCollectConfig.INSTANCE.getTencentClsLogConfig());
        tencentClsLogCollectClient.close();
    }
}
