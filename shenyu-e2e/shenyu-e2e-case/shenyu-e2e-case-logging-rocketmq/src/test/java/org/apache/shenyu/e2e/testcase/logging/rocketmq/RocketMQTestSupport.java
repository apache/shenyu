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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.rocketmq.client.consumer.DefaultMQPullConsumer;
import org.apache.rocketmq.client.consumer.DefaultMQPushConsumer;
import org.apache.rocketmq.client.consumer.PullResult;
import org.apache.rocketmq.client.exception.MQBrokerException;
import org.apache.rocketmq.client.exception.MQClientException;
import org.apache.rocketmq.common.MixAll;
import org.apache.rocketmq.common.TopicConfig;
import org.apache.rocketmq.common.constant.PermName;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.common.message.MessageQueue;
import org.apache.rocketmq.common.protocol.ResponseCode;
import org.apache.rocketmq.common.protocol.body.ConsumerRunningInfo;
import org.apache.rocketmq.common.protocol.body.ProcessQueueInfo;
import org.apache.rocketmq.common.protocol.route.BrokerData;
import org.apache.rocketmq.common.protocol.route.QueueData;
import org.apache.rocketmq.remoting.exception.RemotingException;
import org.apache.rocketmq.tools.admin.DefaultMQAdminExt;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.Callable;

import static org.awaitility.Awaitility.await;

/**
 * Establishes the MQ prerequisites before the access log assertion begins.
 */
final class RocketMQTestSupport implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(RocketMQTestSupport.class);

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final Duration PREPARATION_TIMEOUT = Duration.ofSeconds(60);

    private static final int QUEUE_COUNT = 4;

    private final DefaultMQAdminExt admin = new DefaultMQAdminExt(1000);

    private final String nameserver;

    private final String topic;

    private final String runId;

    private final Set<MessageQueue> expectedQueues = new HashSet<>();

    private final Map<MessageQueue, Long> initialOffsets = new HashMap<>();

    private String lastState = "not started";

    private long deadline;

    RocketMQTestSupport(final String nameserver, final String topic, final String runId) {
        this.nameserver = nameserver;
        this.topic = topic;
        this.runId = runId;
        admin.setNamesrvAddr(nameserver);
        admin.setInstanceName("rocketmq-e2e-admin-" + runId);
    }

    void prepare(final DefaultMQPushConsumer consumer) throws Exception {
        long started = System.nanoTime();
        deadline = started + PREPARATION_TIMEOUT.toNanos();
        try {
            admin.start();
            waitUntilReady("topic route", this::prepareTopic);
            consumer.start();
            waitUntilReady("consumer queues", () -> consumerReady(consumer));
            for (MessageQueue queue : expectedQueues) {
                remaining();
                initialOffsets.put(queue, admin.maxOffset(queue));
            }
        } catch (Exception e) {
            throw new IllegalStateException("RocketMQ preparation failed: nameserver=" + nameserver + ", topic=" + topic + ", " + lastState, e);
        }
        LOG.info("RocketMQ ready: group={}, clientId={}, queues={}, preparationMs={}", consumer.getConsumerGroup(),
                consumer.buildMQClientId(), expectedQueues, Duration.ofNanos(System.nanoTime() - started).toMillis());
    }

    private boolean prepareTopic() throws Exception {
        Map<String, BrokerData> brokers = admin.examineBrokerClusterInfo().getBrokerAddrTable();
        lastState = "registered brokers=" + brokers.keySet();
        if (brokers.isEmpty()) {
            return false;
        }
        if (brokers.size() != 1) {
            throw new IllegalStateException("Expected the single E2E broker, found " + brokers.keySet());
        }
        BrokerData broker = brokers.values().iterator().next();
        String address = broker.getBrokerAddrs().get(MixAll.MASTER_ID);
        if (Objects.isNull(address)) {
            return false;
        }
        TopicConfig config = admin.getAllTopicConfig(address, 1000).getTopicConfigTable().get(topic);
        if (Objects.isNull(config)) {
            config = new TopicConfig(topic, QUEUE_COUNT, QUEUE_COUNT, PermName.PERM_READ | PermName.PERM_WRITE);
            admin.createAndUpdateTopicConfig(address, config);
        }
        if (!PermName.isReadable(config.getPerm()) || !PermName.isWriteable(config.getPerm())
                || config.getReadQueueNums() <= 0 || config.getReadQueueNums() != config.getWriteQueueNums()) {
            throw new IllegalStateException("Expected readable and writable E2E topic queues: " + config);
        }
        Set<MessageQueue> queues = new HashSet<>();
        for (QueueData data : admin.examineTopicRouteInfo(topic).getQueueDatas()) {
            if (broker.getBrokerName().equals(data.getBrokerName()) && PermName.isReadable(data.getPerm()) && PermName.isWriteable(data.getPerm())
                    && data.getReadQueueNums() == config.getReadQueueNums() && data.getWriteQueueNums() == config.getWriteQueueNums()) {
                for (int i = 0; i < data.getReadQueueNums(); i++) {
                    queues.add(new MessageQueue(topic, data.getBrokerName(), i));
                }
            }
        }
        lastState = "topic=" + topic + ", expected queue count=" + config.getReadQueueNums() + ", routes=" + queues;
        expectedQueues.clear();
        expectedQueues.addAll(queues);
        return queues.size() == config.getReadQueueNums();
    }

    private boolean consumerReady(final DefaultMQPushConsumer consumer) throws Exception {
        ConsumerRunningInfo info = admin.getConsumerRunningInfo(consumer.getConsumerGroup(), consumer.buildMQClientId(), false);
        lastState = "expected=" + expectedQueues + ", consumer queues=" + (Objects.isNull(info) ? null : info.getMqTable());
        return Objects.nonNull(info) && hasAllQueues(expectedQueues, info.getMqTable());
    }

    private void waitUntilReady(final String stage, final Callable<Boolean> condition) {
        await().alias("RocketMQ " + stage + "; nameserver=" + nameserver + "; topic=" + topic)
                .pollInterval(Duration.ofMillis(200)).atMost(remaining())
                .until(() -> {
                    try {
                        return condition.call();
                    } catch (MQClientException e) {
                        if (e.getResponseCode() != ResponseCode.TOPIC_NOT_EXIST && e.getResponseCode() != ResponseCode.CONSUMER_NOT_ONLINE) {
                            throw e;
                        }
                        lastState = exceptionSummary(e);
                    } catch (RemotingException e) {
                        lastState = exceptionSummary(e);
                    } catch (MQBrokerException e) {
                        if (e.getResponseCode() != ResponseCode.CONSUMER_NOT_ONLINE && e.getResponseCode() != ResponseCode.TOPIC_NOT_EXIST) {
                            throw e;
                        }
                        lastState = exceptionSummary(e);
                    }
                    LOG.debug("Waiting for RocketMQ {}: {}", stage, lastState);
                    return false;
                });
    }

    private Duration remaining() {
        long nanos = deadline - System.nanoTime();
        if (nanos <= 0) {
            throw new IllegalStateException("RocketMQ preparation timed out: " + lastState);
        }
        return Duration.ofNanos(nanos);
    }

    static boolean hasAllQueues(final Set<MessageQueue> expected, final Map<MessageQueue, ProcessQueueInfo> actual) {
        return !expected.isEmpty() && Objects.nonNull(actual) && expected.stream().allMatch(queue -> {
            ProcessQueueInfo info = actual.get(queue);
            return Objects.nonNull(info) && !info.isDroped();
        });
    }

    static boolean matchesAccessLog(final byte[] body, final String uri) {
        try {
            JsonNode log = MAPPER.readTree(body);
            if (Objects.isNull(log) || !log.path("requestUri").isTextual() || log.path("status").asInt() != 200) {
                return false;
            }
            URI logged = URI.create(log.path("requestUri").asText());
            String path = logged.getRawPath() + (Objects.isNull(logged.getRawQuery()) ? "" : "?" + logged.getRawQuery());
            return uri.equals(path);
        } catch (IOException | IllegalArgumentException e) {
            LOG.warn("Cannot parse RocketMQ access log", e);
            return false;
        }
    }

    String diagnostics(final DefaultMQPushConsumer consumer) {
        StringBuilder result = new StringBuilder("nameserver=").append(nameserver).append(", topic=").append(topic)
                .append(", group=").append(consumer.getConsumerGroup()).append(", lastState=").append(lastState);
        try {
            ConsumerRunningInfo info = admin.getConsumerRunningInfo(consumer.getConsumerGroup(), consumer.buildMQClientId(), false);
            result.append(", currentQueues=").append(Objects.isNull(info) ? null : info.getMqTable());
        } catch (Exception e) {
            result.append(", currentQueues error=").append(exceptionSummary(e));
        }
        try {
            Map<MessageQueue, String> offsets = new HashMap<>();
            admin.examineTopicStats(topic).getOffsetTable().forEach((queue, offset) ->
                    offsets.put(queue, "min=" + offset.getMinOffset() + ", max=" + offset.getMaxOffset() + ", updated=" + offset.getLastUpdateTimestamp()));
            result.append(", brokerOffsets=").append(offsets);
        } catch (Exception e) {
            result.append(", brokerOffsets error=").append(exceptionSummary(e));
        }
        try {
            dumpNewMessages();
        } catch (Exception e) {
            result.append(", message dump error=").append(exceptionSummary(e));
        }
        return result.toString();
    }

    private static String exceptionSummary(final Throwable cause) {
        return cause.toString().replace('\r', ' ').replace('\n', ' ');
    }

    private void dumpNewMessages() throws Exception {
        DefaultMQPullConsumer reader = new DefaultMQPullConsumer("rocketmq-e2e-dump-" + runId);
        reader.setNamesrvAddr(nameserver);
        reader.setInstanceName("rocketmq-e2e-dump-" + runId);
        long end = System.nanoTime() + Duration.ofSeconds(5).toNanos();
        try {
            reader.start();
            for (Map.Entry<MessageQueue, Long> entry : initialOffsets.entrySet()) {
                if (System.nanoTime() >= end) {
                    break;
                }
                PullResult pulled = reader.pull(entry.getKey(), "*", entry.getValue(), 32, 1000);
                if (Objects.nonNull(pulled.getMsgFoundList())) {
                    for (MessageExt message : pulled.getMsgFoundList()) {
                        LOG.error("RocketMQ broker diagnostic: queue={}, offset={}, messageId={}, storeTimestamp={}, body={}", entry.getKey(),
                                message.getQueueOffset(), message.getMsgId(), message.getStoreTimestamp(), new String(message.getBody(), StandardCharsets.UTF_8));
                    }
                }
            }
        } finally {
            reader.shutdown();
        }
    }

    @Override
    public void close() {
        LOG.info("RocketMQ preparation state: {}", lastState);
        admin.shutdown();
    }
}
