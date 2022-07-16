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

package org.apache.shenyu.plugin.logging.kafka.kafka;

import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringSerializer;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;
import org.junit.Ignore;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Properties;

/**
 * The Test Case For RocketMQLogCollectClient.
 */
public class KafkaLogCollectClientTest {

    private final Properties props = new Properties();

    private final PluginData pluginData = new PluginData();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    private KafkaLogCollectClient kafkaLogCollectClient;
    
    @BeforeEach
    public void setUp() {
        this.kafkaLogCollectClient = new KafkaLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"topic\":\"shenyu-access-logging\", \"namesrvAddr\":\"localhost:8082\"}");
        KafkaLogCollectConfig.KafkaLogConfig globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), KafkaLogCollectConfig.KafkaLogConfig.class);
        globalLogConfig.setCompressAlg("LZ4");
        props.put("bootstrap.servers", globalLogConfig.getNamesrvAddr());
        props.put(GenericLoggingConstant.NAMESERVER_ADDRESS, globalLogConfig.getNamesrvAddr());
        props.setProperty(GenericLoggingConstant.TOPIC, globalLogConfig.getTopic());
        props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
        props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
    }

    @Test
    @Ignore
    public void testInitProducer() throws NoSuchFieldException, IllegalAccessException {
        kafkaLogCollectClient.initProducer(props);
        Field field = kafkaLogCollectClient.getClass().getDeclaredField("topic");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(kafkaLogCollectClient), "shenyu-access-logging");
        kafkaLogCollectClient.close();
    }
}
