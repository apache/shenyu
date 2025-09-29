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

package org.apache.shenyu.plugin.logging.kafka.collector;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.client.LogConsumeClient;
import org.apache.shenyu.plugin.logging.common.collector.AbstractLogCollector;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.kafka.cache.KafkaClientCache;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;
import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

/**
 * The Test Case For DefaultLogCollector.
 */
public class KafkaLogCollectorTest {

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @BeforeEach
    public void setUp() {
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        shenyuRequestLog.setSelectorId("1332017966661636096");
    }

    @Test
    public void testAbstractLogCollector() throws Exception {
        KafkaLogCollector.getInstance().start();
        Field field1 = AbstractLogCollector.class.getDeclaredField("started");
        field1.setAccessible(true);
        Assertions.assertEquals(field1.get(KafkaLogCollector.getInstance()).toString(), "true");
        KafkaLogCollector.getInstance().collect(shenyuRequestLog);
        KafkaLogCollector.getInstance().close();
        Field field2 = AbstractLogCollector.class.getDeclaredField("started");
        field2.setAccessible(true);
        Assertions.assertEquals(field2.get(KafkaLogCollector.getInstance()).toString(), "false");
    }

    @Test
    public void testGetLogConsumeClient() {
        LogConsumeClient logConsumeClient = new KafkaLogCollector().getLogConsumeClient();
        Assertions.assertEquals(KafkaLogCollectClient.class, logConsumeClient.getClass());
    }

    @Test
    public void testGetLogConsumeClient2() {
        KafkaClientCache cache = KafkaClientCache.getInstance();
        SelectorData selectorData = SelectorData.builder().id("1332017966661636096")
                .handle("{\n"
                        + "  \"bootstrapServer\": \"127.0.0.1:5672\",\n"
                        + "  \"producerGroup\": \"testGroup\",\n"
                        + "  \"compressAlg\": \"gzip\",\n"
                        + "  \"securityProtocol\": \"PLAINTEXT\",\n"
                        + "  \"saslMechanism\": \"PLAIN\",\n"
                        + "  \"userName\": \"guest\",\n"
                        + "  \"passWord\": \"guest\"\n"
                        + "}")
                .build();
        KafkaLogCollectConfig.LogApiConfig logApiConfig = GsonUtils.getInstance()
                .fromJson(selectorData.getHandle(), KafkaLogCollectConfig.LogApiConfig.class);
        cache.initKafkaClient("1332017966661636096", logApiConfig);
        KafkaLogCollectClient logConsumeClient = new KafkaLogCollector().getLogConsumeClient("1332017966661636096");
        Assert.assertEquals(KafkaLogCollectClient.class, logConsumeClient.getClass());
    }
}
