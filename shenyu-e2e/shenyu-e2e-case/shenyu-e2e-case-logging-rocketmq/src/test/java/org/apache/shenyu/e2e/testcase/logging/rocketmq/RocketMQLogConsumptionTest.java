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

package org.apache.shenyu.e2e.testcase.logging.rocketmq;

import org.apache.rocketmq.client.consumer.DefaultLitePullConsumer;
import org.apache.rocketmq.client.exception.MQClientException;
import org.apache.rocketmq.common.TopicConfig;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.common.message.MessageQueue;
import org.apache.rocketmq.common.protocol.ResponseCode;
import org.apache.rocketmq.common.protocol.body.ClusterInfo;
import org.apache.rocketmq.common.protocol.route.BrokerData;
import org.apache.rocketmq.common.protocol.route.QueueData;
import org.apache.rocketmq.common.protocol.route.TopicRouteData;
import org.apache.rocketmq.tools.admin.DefaultMQAdminExt;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RocketMQLogConsumptionTest {

    @Test
    void preparesConsumerBeforeWaitingAndRejectsUnrelatedLogs() throws MQClientException {
        AtomicInteger discoveries = new AtomicInteger();
        AtomicInteger polls = new AtomicInteger();
        AtomicInteger seeks = new AtomicInteger();
        MessageQueue queue = new MessageQueue("shenyu-access-logging", "broker", 0);
        String endpoint = "/http/order/findById?id=23&e2e=current-request";
        DefaultLitePullConsumer consumer = new DefaultLitePullConsumer("log-fixture") {
            @Override
            public Collection<MessageQueue> fetchMessageQueues(final String topic) {
                discoveries.incrementAndGet();
                return List.of(queue);
            }

            @Override
            public void assign(final Collection<MessageQueue> queues) {
                assertEquals(List.of(queue), queues);
            }

            @Override
            public void seekToBegin(final MessageQueue assignedQueue) {
                assertEquals(queue, assignedQueue);
                seeks.incrementAndGet();
            }

            @Override
            public List<MessageExt> poll(final long timeout) {
                assertTrue(seeks.get() > 0);
                MessageExt message = new MessageExt();
                String uri = polls.incrementAndGet() == 1 ? "/http/order/findById?id=23&e2e=old-request" : endpoint;
                message.setBody(("{\"requestUri\":\"http://gateway" + uri + "\"}").getBytes(StandardCharsets.UTF_8));
                return List.of(message);
            }
        };

        DividePluginCases.prepareConsumer(consumer);
        assertEquals(1, seeks.get());
        DividePluginCases.awaitRequestLog(consumer, endpoint);

        assertEquals(1, discoveries.get());
        assertEquals(1, seeks.get());
        assertEquals(2, polls.get());
    }

    @Test
    void createsTopicAfterBrokerRegistrationAndWaitsForItsRoute() throws Exception {
        QueueData queue = new QueueData();
        queue.setReadQueueNums(1);
        queue.setWriteQueueNums(1);
        TopicRouteData route = new TopicRouteData();
        route.setQueueDatas(List.of(queue));
        AtomicInteger registrations = new AtomicInteger();
        AtomicInteger routes = new AtomicInteger();
        AtomicBoolean created = new AtomicBoolean();
        DefaultMQAdminExt admin = new DefaultMQAdminExt() {
            @Override
            public ClusterInfo examineBrokerClusterInfo() {
                ClusterInfo info = new ClusterInfo();
                info.setBrokerAddrTable(new HashMap<>());
                if (registrations.incrementAndGet() > 1) {
                    info.getBrokerAddrTable().put("broker", new BrokerData("DefaultCluster", "broker", new HashMap<>(Map.of(0L, "localhost:10911"))));
                }
                return info;
            }

            @Override
            public void createAndUpdateTopicConfig(final String address, final TopicConfig config) {
                assertTrue(registrations.get() > 1);
                assertEquals("localhost:10911", address);
                assertEquals("shenyu-access-logging", config.getTopicName());
                created.set(true);
            }

            @Override
            public TopicRouteData examineTopicRouteInfo(final String topic) throws MQClientException {
                assertTrue(created.get());
                if (routes.incrementAndGet() == 1) {
                    throw new MQClientException(ResponseCode.TOPIC_NOT_EXIST, "route has not been published");
                }
                return route;
            }
        };

        DividePluginCases.prepareTopic(admin);

        assertEquals(2, registrations.get());
        assertEquals(2, routes.get());
    }
}
