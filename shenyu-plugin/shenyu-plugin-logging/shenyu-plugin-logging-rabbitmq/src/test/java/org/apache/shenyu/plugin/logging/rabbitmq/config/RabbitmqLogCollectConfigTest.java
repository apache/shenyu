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

package org.apache.shenyu.plugin.logging.rabbitmq.config;

import org.apache.shenyu.plugin.logging.rabbitmq.config.RabbitmqLogCollectConfig.RabbitmqLogConfig;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 * The Test Case For RabbitmqLogCollectConfig.
 */
public class RabbitmqLogCollectConfigTest {

    private RabbitmqLogConfig rabbitmqLogConfig = new RabbitmqLogConfig();

    private final String queueName = "queue.logging.rabbitmq";

    private final String exchangeName = "exchange.logging.rabbitmq";

    private final String host = "localhost";

    private final Integer port = 5671;

    private final String username = "admin";

    private final String password = "admin";

    private final String routingKey = "routingKey";

    private final String exchangeType = "direct";

    private final String virtualHost = "/";
    
    private final Boolean durable = Boolean.TRUE;
    
    private final Boolean exclusive = Boolean.FALSE;
    
    private final Boolean autoDelete = Boolean.FALSE;
    
    private final Map<String, Object> args = new HashMap<>();

    @Test
    public void testPropertiesGetSet() {
        testGetSet(RabbitmqLogConfig::getVirtualHost, RabbitmqLogConfig::setVirtualHost, virtualHost);
        testGetSet(RabbitmqLogConfig::getExchangeType, RabbitmqLogConfig::setExchangeType, exchangeType);
        testGetSet(RabbitmqLogConfig::getRoutingKey, RabbitmqLogConfig::setRoutingKey, routingKey);
        testGetSet(RabbitmqLogConfig::getQueueName, RabbitmqLogConfig::setQueueName, queueName);
        testGetSet(RabbitmqLogConfig::getExchangeName, RabbitmqLogConfig::setExchangeName, exchangeName);
        testGetSet(RabbitmqLogConfig::getHost, RabbitmqLogConfig::setHost, host);
        testGetSet(RabbitmqLogConfig::getPort, RabbitmqLogConfig::setPort, port);
        testGetSet(RabbitmqLogConfig::getUsername, RabbitmqLogConfig::setUsername, username);
        testGetSet(RabbitmqLogConfig::getPassword, RabbitmqLogConfig::setPassword, password);
        testGetSet(RabbitmqLogConfig::getDurable, RabbitmqLogConfig::setDurable, durable);
        testGetSet(RabbitmqLogConfig::getExclusive, RabbitmqLogConfig::setExclusive, exclusive);
        testGetSet(RabbitmqLogConfig::getAutoDelete, RabbitmqLogConfig::setAutoDelete, autoDelete);
        testGetSet(RabbitmqLogConfig::getArgs, RabbitmqLogConfig::setArgs, args);
    }

    /**
     * abstract function to test properties get and set.
     *
     * @param getter Object Get Function
     * @param setter Object Set Function
     * @param value  Object Function Value
     * @param <T>    T
     */
    private <T> void testGetSet(final Function<RabbitmqLogConfig, T> getter,
                                final BiConsumer<RabbitmqLogConfig, T> setter,
                                final T value) {
        Assert.assertNull(getter.apply(rabbitmqLogConfig));
        setter.accept(rabbitmqLogConfig, value);
        Assert.assertEquals(getter.apply(rabbitmqLogConfig), value);
    }

}
