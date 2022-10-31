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

package org.apache.shenyu.plugin.logging.rocketmq.config;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * The Test Case For LogCollectConfig.
 */
public class RocketMQLogCollectConfigTest {

    @Test
    public void testSetLogApiConfigTopic() {
        RocketMQLogCollectConfig.LogApiConfig logApiConfig = new RocketMQLogCollectConfig.LogApiConfig();
        logApiConfig.setTopic("test");
        Assertions.assertEquals(logApiConfig.getTopic(), "test");
    }

    @Test
    public void testSetLogApiConfigSampleRate() {
        RocketMQLogCollectConfig.LogApiConfig logApiConfig = new RocketMQLogCollectConfig.LogApiConfig();
        logApiConfig.setSampleRate("1");
        Assertions.assertEquals(logApiConfig.getSampleRate(), "1");
    }

    @Test
    public void testGetGlobalLogConfigSampleRate() {
        RocketMQLogCollectConfig.RocketMQLogConfig rocketMQLogConfig = new RocketMQLogCollectConfig.RocketMQLogConfig();
        rocketMQLogConfig.setSampleRate("1");
        Assertions.assertEquals(rocketMQLogConfig.getSampleRate(), "1");
    }

    @Test
    public void testSetGlobalLogConfigTopic() {
        RocketMQLogCollectConfig.RocketMQLogConfig rocketMQLogConfig = new RocketMQLogCollectConfig.RocketMQLogConfig();
        rocketMQLogConfig.setTopic("test");
        Assertions.assertEquals(rocketMQLogConfig.getTopic(), "test");
    }

    @Test
    public void testSetGlobalLogConfigMaxResponseBody() {
        RocketMQLogCollectConfig.RocketMQLogConfig rocketMQLogConfig = new RocketMQLogCollectConfig.RocketMQLogConfig();
        rocketMQLogConfig.setMaxResponseBody(5);
        Assertions.assertEquals(rocketMQLogConfig.getMaxResponseBody(), 5);
    }

    @Test
    public void testSetGlobalLogConfigMaxRequestBody() {
        RocketMQLogCollectConfig.RocketMQLogConfig rocketMQLogConfig = new RocketMQLogCollectConfig.RocketMQLogConfig();
        rocketMQLogConfig.setMaxRequestBody(5);
        Assertions.assertEquals(rocketMQLogConfig.getMaxRequestBody(), 5);
    }

    @Test
    public void testSetGlobalLogConfigNamesrvAddr() {
        RocketMQLogCollectConfig.RocketMQLogConfig rocketMQLogConfig = new RocketMQLogCollectConfig.RocketMQLogConfig();
        rocketMQLogConfig.setNamesrvAddr("test");
        Assertions.assertEquals(rocketMQLogConfig.getNamesrvAddr(), "test");
    }

    @Test
    public void testSetGlobalLogConfigProducerGroup() {
        RocketMQLogCollectConfig.RocketMQLogConfig rocketMQLogConfig = new RocketMQLogCollectConfig.RocketMQLogConfig();
        rocketMQLogConfig.setProducerGroup("test");
        Assertions.assertEquals(rocketMQLogConfig.getProducerGroup(), "test");
    }

    @Test
    public void testGetGlobalLogConfig() {
        RocketMQLogCollectConfig rocketMQLogCollectConfig = new RocketMQLogCollectConfig();
        RocketMQLogCollectConfig.RocketMQLogConfig rocketMQLogConfig = new RocketMQLogCollectConfig.RocketMQLogConfig();
        rocketMQLogCollectConfig.setRocketMQLogConfig(rocketMQLogConfig);
        Assertions.assertEquals(rocketMQLogCollectConfig.getRocketMQLogConfig(), rocketMQLogConfig);
    }

    @Test
    public void testCompressAlg() {
        RocketMQLogCollectConfig.RocketMQLogConfig rocketMQLogConfig = new RocketMQLogCollectConfig.RocketMQLogConfig();
        rocketMQLogConfig.setCompressAlg("LZ4");
        Assertions.assertEquals(rocketMQLogConfig.getCompressAlg(), "LZ4");
    }

    @Test
    public void testBufferQueueSize() {
        RocketMQLogCollectConfig.RocketMQLogConfig rocketMQLogConfig = new RocketMQLogCollectConfig.RocketMQLogConfig();
        rocketMQLogConfig.setBufferQueueSize(50000);
        Assertions.assertEquals(rocketMQLogConfig.getBufferQueueSize(), 50000);
    }
}
