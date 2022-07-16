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

package org.apache.shenyu.plugin.aliyun.sls.config;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test cases for LogCollectConfig.
 */
public class AliyunLogCollectConfigTest {

    private final AliyunLogCollectConfig.AliyunSlsLogConfig aliyunSlsLogConfig = new AliyunLogCollectConfig.AliyunSlsLogConfig();

    @Test
    public void testLogCollectorConfigHost() {
        aliyunSlsLogConfig.setHost("10.10.10.10");
        Assertions.assertEquals(aliyunSlsLogConfig.getHost(), "10.10.10.10");
    }

    @Test
    public void testLogCollectorConfigAccessId() {
        aliyunSlsLogConfig.setAccessId("test");
        Assertions.assertEquals(aliyunSlsLogConfig.getAccessId(), "test");
    }

    @Test
    public void testLogCollectorConfigAccessKey() {
        aliyunSlsLogConfig.setAccessKey("testKey");
        Assertions.assertEquals(aliyunSlsLogConfig.getAccessKey(), "testKey");
    }

    @Test
    public void testLogCollectorConfigProjectName() {
        Assertions.assertEquals(aliyunSlsLogConfig.getProjectName(), "shenyu");
        aliyunSlsLogConfig.setProjectName("shenyu-gateway");
        Assertions.assertEquals(aliyunSlsLogConfig.getProjectName(), "shenyu-gateway");
    }

    @Test
    public void testLogCollectorConfigLogStoreName() {
        Assertions.assertEquals(aliyunSlsLogConfig.getLogStoreName(), "shenyu-logstore");
        aliyunSlsLogConfig.setLogStoreName("shenyu-gateway-logstore");
        Assertions.assertEquals(aliyunSlsLogConfig.getLogStoreName(), "shenyu-gateway-logstore");
    }

    @Test
    public void testLogCollectorConfigTtlInDay() {
        aliyunSlsLogConfig.setTtlInDay(3);
        Assertions.assertEquals(aliyunSlsLogConfig.getTtlInDay(), 3);
    }

    @Test
    public void testLogCollectorConfigShardCount() {
        aliyunSlsLogConfig.setShardCount(3);
        Assertions.assertEquals(aliyunSlsLogConfig.getShardCount(), 3);
    }

    @Test
    public void testLogCollectorConfigTopic() {
        aliyunSlsLogConfig.setTopic("test");
        Assertions.assertEquals(aliyunSlsLogConfig.getTopic(), "test");
    }

    @Test
    public void testThread() {
        aliyunSlsLogConfig.setSendThreadCount(3);
        aliyunSlsLogConfig.setIoThreadCount(3);
        Assertions.assertEquals(aliyunSlsLogConfig.getSendThreadCount(), 3);
        Assertions.assertEquals(aliyunSlsLogConfig.getIoThreadCount(), 3);
    }

}
