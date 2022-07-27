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

package org.apache.shenyu.plugin.logging.kafka.config;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * The Test Case For LogCollectConfig.
 */
public class KafkaLogCollectConfigTest {

    @Test
    public void testSetLogApiConfigTopic() {
        KafkaLogCollectConfig.LogApiConfig logApiConfig = new KafkaLogCollectConfig.LogApiConfig();
        logApiConfig.setTopic("test");
        Assertions.assertEquals(logApiConfig.getTopic(), "test");
    }

    @Test
    public void testSetLogApiConfigSampleRate() {
        KafkaLogCollectConfig.LogApiConfig logApiConfig = new KafkaLogCollectConfig.LogApiConfig();
        logApiConfig.setSampleRate("1");
        Assertions.assertEquals(logApiConfig.getSampleRate(), "1");
    }

    @Test
    public void testGetGlobalLogConfigSampleRate() {
        KafkaLogCollectConfig.KafkaLogConfig kafkaLogConfig = new KafkaLogCollectConfig.KafkaLogConfig();
        kafkaLogConfig.setSampleRate("1");
        Assertions.assertEquals(kafkaLogConfig.getSampleRate(), "1");
    }

    @Test
    public void testSetGlobalLogConfigTopic() {
        KafkaLogCollectConfig.KafkaLogConfig kafkaLogConfig = new KafkaLogCollectConfig.KafkaLogConfig();
        kafkaLogConfig.setTopic("test");
        Assertions.assertEquals(kafkaLogConfig.getTopic(), "test");
    }

    @Test
    public void testSetGlobalLogConfigMaxResponseBody() {
        KafkaLogCollectConfig.KafkaLogConfig kafkaLogConfig = new KafkaLogCollectConfig.KafkaLogConfig();
        kafkaLogConfig.setMaxResponseBody(5);
        Assertions.assertEquals(kafkaLogConfig.getMaxResponseBody(), 5);
    }

    @Test
    public void testSetGlobalLogConfigMaxRequestBody() {
        KafkaLogCollectConfig.KafkaLogConfig kafkaLogConfig = new KafkaLogCollectConfig.KafkaLogConfig();
        kafkaLogConfig.setMaxRequestBody(5);
        Assertions.assertEquals(kafkaLogConfig.getMaxRequestBody(), 5);
    }

    @Test
    public void testSetGlobalLogConfigNamesrvAddr() {
        KafkaLogCollectConfig.KafkaLogConfig kafkaLogConfig = new KafkaLogCollectConfig.KafkaLogConfig();
        kafkaLogConfig.setNamesrvAddr("test");
        Assertions.assertEquals(kafkaLogConfig.getNamesrvAddr(), "test");
    }

    @Test
    public void testSetGlobalLogConfigProducerGroup() {
        KafkaLogCollectConfig.KafkaLogConfig kafkaLogConfig = new KafkaLogCollectConfig.KafkaLogConfig();
        kafkaLogConfig.setProducerGroup("test");
        Assertions.assertEquals(kafkaLogConfig.getProducerGroup(), "test");
    }

    @Test
    public void testGetGlobalLogConfig() {
        KafkaLogCollectConfig kafkaLogCollectConfig = new KafkaLogCollectConfig();
        KafkaLogCollectConfig.KafkaLogConfig kafkaLogConfig = new KafkaLogCollectConfig.KafkaLogConfig();
        kafkaLogCollectConfig.setKafkaLogConfig(kafkaLogConfig);
        Assertions.assertEquals(kafkaLogCollectConfig.getKafkaLogConfig(), kafkaLogConfig);
    }
}
