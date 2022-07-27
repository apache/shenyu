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

package org.apache.shenyu.plugin.aliyun.sls.aliyunsls;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.aliyun.sls.client.AliyunSlsLogCollectClient;
import org.apache.shenyu.plugin.aliyun.sls.config.AliyunLogCollectConfig;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * test cases for AliyunSlsLogCollectClient.
 */
public class AliyunSlsLogCollectClientTest {

    private AliyunSlsLogCollectClient aliyunSlsLogCollectClient;

    private final Properties props = new Properties();

    private final PluginData pluginData = new PluginData();

    private AliyunLogCollectConfig.AliyunSlsLogConfig aliyunSlsLogConfig;

    private final List<ShenyuRequestLog> logs = new ArrayList<>();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @BeforeEach
    public void setup() {
        this.aliyunSlsLogCollectClient = new AliyunSlsLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"topic\":\"shenyu-topic-test\", \"accessId\":\"test\", \"accessKey\":\"test\", "
                + "\"host\":\"cn-guangzhou.log.aliyuncs.com\", \"projectName\":\"shenyu-test\", \"logStoreName\":\"shenyu-test-logstore\"}");
        aliyunSlsLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(),
                AliyunLogCollectConfig.AliyunSlsLogConfig.class);
        props.setProperty(GenericLoggingConstant.HOST, aliyunSlsLogConfig.getHost());
        props.setProperty(GenericLoggingConstant.ACCESS_ID, aliyunSlsLogConfig.getAccessId());
        props.setProperty(GenericLoggingConstant.ACCESS_KEY, aliyunSlsLogConfig.getAccessKey());
        props.setProperty(GenericLoggingConstant.PROJECT_NAME, aliyunSlsLogConfig.getProjectName().trim());
        props.setProperty(GenericLoggingConstant.LOG_STORE, aliyunSlsLogConfig.getLogStoreName().trim());
        props.setProperty(GenericLoggingConstant.TTL_IN_DAY, String.valueOf(aliyunSlsLogConfig.getTtlInDay()));
        props.setProperty(GenericLoggingConstant.SHARD_COUNT, String.valueOf(aliyunSlsLogConfig.getShardCount()));
        props.setProperty(GenericLoggingConstant.TOPIC, aliyunSlsLogConfig.getTopic().trim());
        props.setProperty(GenericLoggingConstant.SEND_THREAD_COUNT, String.valueOf(aliyunSlsLogConfig.getSendThreadCount()));
        props.setProperty(GenericLoggingConstant.IO_THREAD_COUNT, String.valueOf(aliyunSlsLogConfig.getIoThreadCount()));
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        logs.add(shenyuRequestLog);
    }

    @Test
    public void testInitClient() throws NoSuchFieldException, IllegalAccessException {
        aliyunSlsLogCollectClient.initClient(props);
        Field field = aliyunSlsLogCollectClient.getClass().getDeclaredField("topic");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(aliyunSlsLogCollectClient), "shenyu-topic-test");
        Field accessId = aliyunSlsLogCollectClient.getClass().getDeclaredField("projectName");
        accessId.setAccessible(true);
        Assertions.assertEquals(accessId.get(aliyunSlsLogCollectClient), "shenyu-test");
        aliyunSlsLogCollectClient.close();
    }

    @Test
    public void testConsume() {
        String msg = "";
        AliyunLogCollectConfig.INSTANCE.setAliyunSlsLogConfig(aliyunSlsLogConfig);
        aliyunSlsLogCollectClient.initClient(props);
        try {
            aliyunSlsLogCollectClient.consume(logs);
        } catch (Exception e) {
            msg = "false";
        }
        Assertions.assertEquals(msg, "");
        aliyunSlsLogCollectClient.close();
    }
}
