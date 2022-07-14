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

package org.apache.shenyu.plugin.logging.rocketmq.client;

import net.jpountz.lz4.LZ4Compressor;
import net.jpountz.lz4.LZ4Factory;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.rocketmq.client.producer.DefaultMQProducer;
import org.apache.rocketmq.common.message.Message;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.common.client.LogConsumeClient;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.LZ4CompressData;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.logging.rocketmq.config.RocketMQLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * queue-based logging collector.
 */
public class RocketMQLogCollectClient implements LogConsumeClient {

    private static final Logger LOG = LoggerFactory.getLogger(RocketMQLogCollectClient.class);

    private static Map<String, String> apiTopicMap = new HashMap<>();

    private static final String DEFAULT_PRODUCER_GROUP = "shenyu-plugin-logging-rocketmq";

    private DefaultMQProducer producer;

    private String topic;

    private final AtomicBoolean isStarted = new AtomicBoolean(false);

    /**
     * init producer.
     *
     * @param props rocketmq props
     */
    public void initProducer(final Properties props) {
        if (MapUtils.isEmpty(props)) {
            LOG.error("RocketMQ props is empty. failed init RocketMQ producer");
            return;
        }
        if (isStarted.get()) {
            close();
        }
        String topic = props.getProperty(GenericLoggingConstant.TOPIC);
        String nameserverAddress = props.getProperty(GenericLoggingConstant.NAMESERVER_ADDRESS);
        String producerGroup = props.getProperty(GenericLoggingConstant.PRODUCER_GROUP, DEFAULT_PRODUCER_GROUP);
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
            isStarted.set(true);
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
        if (CollectionUtils.isEmpty(logs) || !isStarted.get()) {
            return;
        }
        logs.forEach(log -> {
            String logTopic = StringUtils.defaultIfBlank(LogCollectConfigUtils.getTopic(log.getPath(), apiTopicMap), topic);
            try {
                producer.sendOneway(toMessage(logTopic, log));
            } catch (Exception e) {
                LOG.error("rocketmq push logs error", e);
            }
        });
    }

    private Message toMessage(final String logTopic, final ShenyuRequestLog log) {
        byte[] bytes = JsonUtils.toJson(log).getBytes(StandardCharsets.UTF_8);
        String compressAlg = StringUtils.defaultIfBlank(RocketMQLogCollectConfig.INSTANCE.getRocketMQLogConfig().getCompressAlg(), "");
        if ("LZ4".equalsIgnoreCase(compressAlg.trim())) {
            LZ4CompressData lz4CompressData = new LZ4CompressData(bytes.length, compressedByte(bytes));
            return new Message(logTopic, JsonUtils.toJson(lz4CompressData).getBytes(StandardCharsets.UTF_8));
        } else {
            return new Message(logTopic, bytes);
        }
    }

    private byte[] compressedByte(final byte[] srcByte) {
        LZ4Factory factory = LZ4Factory.fastestInstance();
        LZ4Compressor compressor = factory.fastCompressor();
        return compressor.compress(srcByte);
    }

    /**
     * set api topic map.
     * @param uriTopicMap api topic map
     */
    public static void setTopic(final Map<String, String> uriTopicMap) {
        apiTopicMap = uriTopicMap;
    }

    /**
     * close producer.
     */
    @Override
    public void close() {
        if (Objects.nonNull(producer) && isStarted.get()) {
            producer.shutdown();
            isStarted.set(false);
        }
    }
}
