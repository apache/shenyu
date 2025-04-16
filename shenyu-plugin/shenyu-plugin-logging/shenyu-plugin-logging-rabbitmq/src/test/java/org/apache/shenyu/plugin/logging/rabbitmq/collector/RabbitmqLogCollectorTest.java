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

package org.apache.shenyu.plugin.logging.rabbitmq.collector;

import org.apache.shenyu.plugin.logging.common.collector.AbstractLogCollector;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.rabbitmq.client.RabbitmqLogCollectClient;
import org.apache.shenyu.plugin.logging.rabbitmq.conllector.RabbitmqLogCollector;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;

/**
 * The Test Case For RabbitmqLogCollector.
 */
public class RabbitmqLogCollectorTest {

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @Before
    public void setUp() {
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
    }

    @Test
    public void testAbstractLogCollector() throws Exception {
        // collector start
        RabbitmqLogCollector.getInstance().start();
        Field field1 = AbstractLogCollector.class.getDeclaredField("started");
        field1.setAccessible(true);

        Assert.assertEquals(field1.get(RabbitmqLogCollector.getInstance()).toString(), Boolean.TRUE.toString());

        RabbitmqLogCollector.getInstance().collect(shenyuRequestLog);
        RabbitmqLogCollector.getInstance().close();
        // collector closed
        Field field2 = AbstractLogCollector.class.getDeclaredField("started");
        field2.setAccessible(true);
        Assert.assertEquals(field2.get(RabbitmqLogCollector.getInstance()).toString(), Boolean.FALSE.toString());
    }

    @Test
    public void testGetLogConsumeClient() {
        RabbitmqLogCollectClient logConsumeClient = new RabbitmqLogCollector().getLogConsumeClient();
        Assert.assertEquals(RabbitmqLogCollectClient.class, logConsumeClient.getClass());
    }

}
