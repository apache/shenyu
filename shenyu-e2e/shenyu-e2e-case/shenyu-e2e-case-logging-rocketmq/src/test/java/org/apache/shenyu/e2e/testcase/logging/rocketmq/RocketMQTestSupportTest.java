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

import org.apache.rocketmq.common.message.MessageQueue;
import org.apache.rocketmq.common.protocol.body.ProcessQueueInfo;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RocketMQTestSupportTest {

    @Test
    void requiresEveryTargetQueueAndRejectsRetryQueues() {
        MessageQueue first = new MessageQueue("access-log", "broker", 0);
        MessageQueue second = new MessageQueue("access-log", "broker", 1);
        MessageQueue retry = new MessageQueue("%RETRY%test", "broker", 0);
        Set<MessageQueue> expected = Set.of(first, second);
        assertFalse(RocketMQTestSupport.hasAllQueues(expected, Map.of(retry, new ProcessQueueInfo())));
        assertFalse(RocketMQTestSupport.hasAllQueues(expected, Map.of(first, new ProcessQueueInfo())));
        assertTrue(RocketMQTestSupport.hasAllQueues(expected, Map.of(first, new ProcessQueueInfo(), second, new ProcessQueueInfo())));
        assertFalse(RocketMQTestSupport.hasAllQueues(Set.of(), Map.of()));
    }

    @Test
    void rejectsMissingQueueState() {
        MessageQueue queue = new MessageQueue("access-log", "broker", 0);
        assertFalse(RocketMQTestSupport.hasAllQueues(Set.of(queue), null));
        Map<MessageQueue, ProcessQueueInfo> actual = new HashMap<>();
        actual.put(queue, null);
        assertFalse(RocketMQTestSupport.hasAllQueues(Set.of(queue), actual));
    }

    @Test
    void rejectsDroppedTargetQueue() {
        MessageQueue queue = new MessageQueue("access-log", "broker", 0);
        ProcessQueueInfo dropped = new ProcessQueueInfo();
        dropped.setDroped(true);
        assertFalse(RocketMQTestSupport.hasAllQueues(Set.of(queue), Map.of(queue, dropped)));
    }

    @Test
    void onlyAcceptsSuccessfulLogForThisRequest() {
        String uri = "/http/order/findById?id=rocketmq-e2e-new";
        assertTrue(RocketMQTestSupport.matchesAccessLog(("{\"requestUri\":\"http://localhost:31195" + uri + "\",\"status\":200}").getBytes(StandardCharsets.UTF_8), uri));
        assertFalse(RocketMQTestSupport.matchesAccessLog("{\"requestUri\":\"/http/order/findById?id=rocketmq-e2e-old\",\"status\":200}".getBytes(StandardCharsets.UTF_8), uri));
        assertFalse(RocketMQTestSupport.matchesAccessLog(("{\"requestUri\":\"" + uri + "\",\"status\":500}").getBytes(StandardCharsets.UTF_8), uri));
        assertFalse(RocketMQTestSupport.matchesAccessLog(("{\"uri\":\"" + uri + "\",\"status\":200}").getBytes(StandardCharsets.UTF_8), uri));
        assertFalse(RocketMQTestSupport.matchesAccessLog(("{\"body\":\"" + uri + "\",\"status\":200}").getBytes(StandardCharsets.UTF_8), uri));
        assertFalse(RocketMQTestSupport.matchesAccessLog("null".getBytes(StandardCharsets.UTF_8), uri));
    }
}
