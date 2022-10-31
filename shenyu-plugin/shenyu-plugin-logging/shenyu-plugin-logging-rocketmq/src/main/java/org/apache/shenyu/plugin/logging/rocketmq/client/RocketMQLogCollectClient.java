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
import org.apache.commons.lang3.StringUtils;
import org.apache.rocketmq.acl.common.AclClientRPCHook;
import org.apache.rocketmq.acl.common.SessionCredentials;
import org.apache.rocketmq.client.producer.DefaultMQProducer;
import org.apache.rocketmq.common.message.Message;
import org.apache.rocketmq.remoting.RPCHook;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.entity.LZ4CompressData;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.logging.rocketmq.config.RocketMQLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * queue-based logging collector.
 */
public class RocketMQLogCollectClient extends AbstractLogConsumeClient<RocketMQLogCollectConfig.RocketMQLogConfig, ShenyuRequestLog> {

    private static final Logger LOG = LoggerFactory.getLogger(RocketMQLogCollectClient.class);

    private static Map<String, String> apiTopicMap = new HashMap<>();

    private static final String DEFAULT_PRODUCER_GROUP = "shenyu-plugin-logging-rocketmq";

    private DefaultMQProducer producer;

    private String topic;

    /**
     * init producer.
     *
     * @param config rocketmq props
     */
    @Override
    public void initClient0(@NonNull final RocketMQLogCollectConfig.RocketMQLogConfig config) {
        String topic = config.getTopic();
        String nameserverAddress = config.getNamesrvAddr();
        String producerGroup = config.getProducerGroup();
        producerGroup = Optional.ofNullable(producerGroup).orElse(DEFAULT_PRODUCER_GROUP);
        if (StringUtils.isBlank(topic) || StringUtils.isBlank(nameserverAddress)) {
            LOG.error("init RocketMQLogCollectClient error, please check topic or nameserverAddress");
            return;
        }
        this.topic = topic;
        producer = new DefaultMQProducer(producerGroup, getAclRPCHook(config));
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
     * get Acl(Access Control List) rpc Hook.
     * @param config rocketMqLog Config
     * @return boolean
     */
    private RPCHook getAclRPCHook(final RocketMQLogCollectConfig.RocketMQLogConfig config) {
        if (StringUtils.isBlank(config.getAccessKey()) || StringUtils.isBlank(config.getSecretKey())) {
            return null;
        }
        return new AclClientRPCHook(new SessionCredentials(config.getAccessKey(), config.getSecretKey()));
    }

    /**
     * store logs.
     *
     * @param logs list of log
     */
    @Override
    public void consume0(@NonNull final List<ShenyuRequestLog> logs) {
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
    public void close0() {
        if (Objects.nonNull(producer)) {
            producer.shutdown();
        }
    }
}
