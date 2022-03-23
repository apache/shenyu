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

package org.apache.shenyu.plugin.logging.rocketmq;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.rocketmq.client.producer.DefaultMQProducer;
import org.apache.rocketmq.common.message.Message;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.LogConsumeClient;
import org.apache.shenyu.plugin.logging.constant.LoggingConstant;
import org.apache.shenyu.plugin.logging.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.utils.LogCollectConfigUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Properties;

/**
 * queue-based logging collector.
 */
public class RocketMQLogCollectClient implements LogConsumeClient {

    private static final Logger LOG = LoggerFactory.getLogger(RocketMQLogCollectClient.class);

    private static final String DEFAULT_PRODUCER_GROUP = "shenyu-plugin-logging-rocketmq";

    private DefaultMQProducer producer;

    private String topic;

    public RocketMQLogCollectClient(final Properties rocketmqProps) {
        initProducer(rocketmqProps);
    }

    /**
     * init producer.
     *
     * @param props rocketmq props
     */
    private void initProducer(final Properties props) {
        if (MapUtils.isEmpty(props)) {
            LOG.error("RocketMQ props is empty. failed init RocketMQ producer");
            return;
        }
        String topic = props.getProperty(LoggingConstant.TOPIC);
        String nameserverAddress = props.getProperty(LoggingConstant.NAMESERVER_ADDRESS);
        String producerGroup = props.getProperty(LoggingConstant.PRODUCER_GROUP, DEFAULT_PRODUCER_GROUP);
        if (StringUtils.isBlank(topic) || StringUtils.isBlank(nameserverAddress)) {
            LOG.error("init RocketMQLogCollectClient error, please check topic or nameserverAddress");
            return;
        }
        this.topic = topic;
        producer = new DefaultMQProducer(producerGroup);
        producer.setNamesrvAddr(nameserverAddress);
        producer.setRetryTimesWhenSendAsyncFailed(0);
        producer.setInstanceName(DEFAULT_PRODUCER_GROUP);
        try {
            producer.start();
            LOG.info("init RocketMQLogCollectClient success");
            Runtime.getRuntime().addShutdownHook(new Thread(this::close));
        } catch (Exception e) {
            LOG.error("init RocketMQLogCollectClient error", e);
        }
    }

    /**
     * store logs.
     *
     * @param logs list of log
     */
    @Override
    public void consume(final List<ShenyuRequestLog> logs) {
        if (CollectionUtils.isEmpty(logs)) {
            return;
        }
        logs.forEach(log -> {
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
     */
    @Override
    public void close() {
        if (producer != null) {
            producer.shutdown();
        }
    }
}
