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
import org.apache.shenyu.plugin.aliyun.sls.config.LogCollectConfig;
import org.apache.shenyu.plugin.aliyun.sls.constant.LoggingConstant;
import org.apache.shenyu.plugin.aliyun.sls.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.aliyun.sls.utils.LogCollectConfigUtils;
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

    private Properties props = new Properties();

    private PluginData pluginData = new PluginData();

    private LogCollectConfig.GlobalLogConfig globalLogConfig;

    private List<ShenyuRequestLog> logs = new ArrayList<>();

    private ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @BeforeEach
    public void setup() {
        this.aliyunSlsLogCollectClient = new AliyunSlsLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"topic\":\"shenyu-topic-test\", \"accessId\":\"test\", \"accessKey\":\"test\", "
                + "\"host\":\"cn-guangzhou.log.aliyuncs.com\", \"projectName\":\"shenyu-test\", \"logStoreName\":\"shenyu-test-logstore\"}");
        globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(),
                LogCollectConfig.GlobalLogConfig.class);
        props.setProperty(LoggingConstant.HOST, globalLogConfig.getHost());
        props.setProperty(LoggingConstant.ACCESS_ID, globalLogConfig.getAccessId());
        props.setProperty(LoggingConstant.ACCESS_KEY, globalLogConfig.getAccessKey());
        props.setProperty(LoggingConstant.PROJECT_NAME, globalLogConfig.getProjectName().trim());
        props.setProperty(LoggingConstant.LOG_STORE, globalLogConfig.getLogStoreName().trim());
        props.setProperty(LoggingConstant.TTL_IN_DAY, String.valueOf(globalLogConfig.getTtlInDay()));
        props.setProperty(LoggingConstant.SHARD_COUNT, String.valueOf(globalLogConfig.getShardCount()));
        props.setProperty(LoggingConstant.TOPIC, globalLogConfig.getTopic().trim());
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
        LogCollectConfigUtils.setGlobalConfig(globalLogConfig);
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
