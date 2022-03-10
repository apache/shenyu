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

package org.apache.shenyu.agent.plugin.logging.rocketmq;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.rocketmq.client.producer.DefaultMQProducer;
import org.apache.rocketmq.common.message.Message;
import org.apache.shenyu.agent.api.config.AgentPluginConfig;
import org.apache.shenyu.agent.plugin.logging.LogConsumeClient;
import org.apache.shenyu.agent.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.agent.plugin.logging.constant.LoggingConstant;
import org.apache.shenyu.agent.plugin.logging.entity.ShenyuRequestLog;
import org.apache.shenyu.common.utils.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.Properties;

/**
 * queue-based logging collector.
 */
public class RocketMQLogCollectClient implements LogConsumeClient {

    private static final Logger LOG = LoggerFactory.getLogger(RocketMQLogCollectClient.class);

    private DefaultMQProducer producer;

    private String topic;

    public RocketMQLogCollectClient(final AgentPluginConfig agentPluginConfig) {
        Objects.requireNonNull(agentPluginConfig);
        initProducer(agentPluginConfig);
    }

    /**
     * init producer.
     *
     * @param agentPluginConfig config
     */
    private void initProducer(final AgentPluginConfig agentPluginConfig) {
        Objects.requireNonNull(agentPluginConfig);
        Properties props = Objects.requireNonNull(agentPluginConfig.getProps());
        String topic = props.getProperty(LoggingConstant.TOPIC);
        String nameserverAddress = props.getProperty(LoggingConstant.NAMESERVER_ADDRESS);
        String producerGroup = props.getProperty(LoggingConstant.PRODUCER_GROUP);
        if (StringUtils.isBlank(topic) || StringUtils.isBlank(nameserverAddress) || StringUtils.isBlank(producerGroup)) {
            throw new IllegalStateException("init RocketMQLogCollectClient error, please agentPluginConfig props");
        }
        this.topic = topic;
        producer = new DefaultMQProducer(producerGroup);
        producer.setNamesrvAddr(nameserverAddress);
        producer.setRetryTimesWhenSendAsyncFailed(0);
        producer.setInstanceName("shenyu-agent-logging-mq-producer");
        try {
            producer.start();
            LOG.info("init RocketMQLogCollectClient success");
        } catch (Exception e) {
            LOG.error("init RocketMQLogCollectClient error", e);
        }
    }

    /**
     * store logs.
     *
     * @param logs list of log
     * @throws Exception produce exception
     */
    @Override
    public void consume(final List<ShenyuRequestLog> logs) throws Exception {
        if (CollectionUtils.isEmpty(logs)) {
            return;
        }
        logs.stream().forEach(log -> {
            String logTopic = StringUtils.defaultIfBlank(LogCollectConfigUtils.getTopic(log.getPath()), topic);
            Message message = new Message(logTopic, JsonUtils.toJson(log).getBytes(StandardCharsets.UTF_8));
            try {
                producer.sendOneway(message);
            } catch (Exception e) {
                LOG.error("rocketmq push logs error", e);
            }
        });
    }

    /**
     * close producer.
     *
     * @throws Exception producer Exception
     */
    @Override
    public void close() throws Exception {
        producer.shutdown();
    }
}
