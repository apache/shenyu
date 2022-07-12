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
public class LogCollectConfigTest {

    private final LogCollectConfig.GlobalLogConfig globalLogConfig = new LogCollectConfig.GlobalLogConfig();

    @Test
    public void testLogCollectorConfigHost() {
        globalLogConfig.setHost("10.10.10.10");
        Assertions.assertEquals(globalLogConfig.getHost(), "10.10.10.10");
    }

    @Test
    public void testLogCollectorConfigAccessId() {
        globalLogConfig.setAccessId("test");
        Assertions.assertEquals(globalLogConfig.getAccessId(), "test");
    }

    @Test
    public void testLogCollectorConfigAccessKey() {
        globalLogConfig.setAccessKey("testKey");
        Assertions.assertEquals(globalLogConfig.getAccessKey(), "testKey");
    }

    @Test
    public void testLogCollectorConfigProjectName() {
        Assertions.assertEquals(globalLogConfig.getProjectName(), "shenyu");
        globalLogConfig.setProjectName("shenyu-gateway");
        Assertions.assertEquals(globalLogConfig.getProjectName(), "shenyu-gateway");
    }

    @Test
    public void testLogCollectorConfigLogStoreName() {
        Assertions.assertEquals(globalLogConfig.getLogStoreName(), "shenyu-logstore");
        globalLogConfig.setLogStoreName("shenyu-gateway-logstore");
        Assertions.assertEquals(globalLogConfig.getLogStoreName(), "shenyu-gateway-logstore");
    }

    @Test
    public void testLogCollectorConfigTtlInDay() {
        globalLogConfig.setTtlInDay(3);
        Assertions.assertEquals(globalLogConfig.getTtlInDay(), 3);
    }

    @Test
    public void testLogCollectorConfigShardCount() {
        globalLogConfig.setShardCount(3);
        Assertions.assertEquals(globalLogConfig.getShardCount(), 3);
    }

    @Test
    public void testLogCollectorConfigTopic() {
        globalLogConfig.setTopic("test");
        Assertions.assertEquals(globalLogConfig.getTopic(), "test");
    }

    @Test
    public void testThread() {
        globalLogConfig.setSendThreadCount(3);
        globalLogConfig.setIoThreadCount(3);
        Assertions.assertEquals(globalLogConfig.getSendThreadCount(), 3);
        Assertions.assertEquals(globalLogConfig.getIoThreadCount(), 3);
    }

}
