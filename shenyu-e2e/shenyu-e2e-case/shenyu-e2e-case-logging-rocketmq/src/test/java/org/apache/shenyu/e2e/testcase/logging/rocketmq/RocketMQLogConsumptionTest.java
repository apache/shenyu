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
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.common.message.MessageQueue;
import org.apache.rocketmq.common.protocol.ResponseCode;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RocketMQLogConsumptionTest {

    @Test
    void rejectsNonTransientBrokerErrors() {
        DefaultLitePullConsumer consumer = new DefaultLitePullConsumer("denied-log-fixture") {
            @Override
            public Collection<MessageQueue> fetchMessageQueues(final String topic) throws MQClientException {
                throw new MQClientException(ResponseCode.NO_PERMISSION, "access denied");
            }
        };

        MQClientException exception = assertThrows(MQClientException.class, () -> DividePluginCases.awaitRequestLog(consumer, "/current-request"));
        assertEquals(ResponseCode.NO_PERMISSION, exception.getResponseCode());
    }

    @Test
    void waitsForTopicCreationAndRejectsUnrelatedLogs() {
        AtomicInteger discoveries = new AtomicInteger();
        AtomicInteger polls = new AtomicInteger();
        AtomicInteger seeks = new AtomicInteger();
        MessageQueue queue = new MessageQueue("shenyu-access-logging", "broker", 0);
        String endpoint = "/http/order/findById?id=23&e2e=current-request";
        DefaultLitePullConsumer consumer = new DefaultLitePullConsumer("log-fixture") {
            @Override
            public Collection<MessageQueue> fetchMessageQueues(final String topic) throws MQClientException {
                if (discoveries.incrementAndGet() == 1) {
                    throw new MQClientException("topic route pending", new MQClientException(ResponseCode.TOPIC_NOT_EXIST, "topic is not published yet"));
                }
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

        DividePluginCases.awaitRequestLog(consumer, endpoint);

        assertEquals(2, discoveries.get());
        assertEquals(1, seeks.get());
        assertEquals(2, polls.get());
    }
}
