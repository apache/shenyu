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

package org.apache.shenyu.integrated.test.http.combination;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.rabbitmq.client.RabbitmqLogCollectClient;
import org.apache.shenyu.plugin.logging.rabbitmq.config.RabbitmqLogCollectConfig;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Integration Test for logging-rabbitmq plugin.
 */
public final class LoggingRabbitMqPluginTest extends AbstractPluginDataInit {

    private final RabbitmqLogCollectClient rabbitmqLogCollectClient = new RabbitmqLogCollectClient();

    private final PluginData pluginData = new PluginData();

    private final List<ShenyuRequestLog> logs = new ArrayList<>();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @BeforeEach
    public void setUp() throws IOException {
        String pluginResult = initPlugin(PluginEnum.LOGGING_RABBITMQ.getName(),
                "{\"host\":\"127.0.0.1\",\"port\":5672,\"password\":\"admin\",\"username\":\"admin\"," 
                        + "\"exchangeName\":\"exchange.logging.plugin\",\"queueName\":\"queue.logging.plugin\"," 
                        + "\"routingKey\":\"topic.logging\",\"virtualHost\":\"/\",\"exchangeType\":\"direct\"," 
                        + "\"durable\":\"true\",\"exclusive\":\"false\",\"autoDelete\":\"false\"}");
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void testPass() {
        final RabbitmqLogCollectConfig.RabbitmqLogConfig rabbitmqLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), RabbitmqLogCollectConfig.RabbitmqLogConfig.class);

        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        logs.add(shenyuRequestLog);

        String msg = "";
        RabbitmqLogCollectConfig.INSTANCE.setRabbitmqLogConfig(rabbitmqLogConfig);
        try {
            rabbitmqLogCollectClient.consume(logs);
        } catch (Exception e) {
            msg = "false";
        }

        assertEquals(msg, "");
        rabbitmqLogCollectClient.close();
    }

    @AfterAll
    public static void clean() throws IOException {
        cleanPluginData(PluginEnum.LOGGING_RABBITMQ.getName());
    }
}
