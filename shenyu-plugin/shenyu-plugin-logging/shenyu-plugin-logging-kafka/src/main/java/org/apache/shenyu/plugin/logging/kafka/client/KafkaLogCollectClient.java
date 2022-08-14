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

package org.apache.shenyu.plugin.logging.kafka.client;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import net.jpountz.lz4.LZ4Compressor;
import net.jpountz.lz4.LZ4Factory;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.common.KafkaException;
import org.apache.kafka.common.errors.AuthorizationException;
import org.apache.kafka.common.errors.OutOfOrderSequenceException;
import org.apache.kafka.common.errors.ProducerFencedException;
import org.apache.kafka.common.serialization.StringSerializer;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.common.client.LogConsumeClient;
import org.apache.shenyu.plugin.logging.common.entity.LZ4CompressData;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * queue-based logging collector.
 */
public class KafkaLogCollectClient implements LogConsumeClient {

    private static final Logger LOG = LoggerFactory.getLogger(KafkaLogCollectClient.class);

    private static Map<String, String> apiTopicMap = new HashMap<>();

    private final AtomicBoolean isStarted = new AtomicBoolean(false);

    private KafkaProducer<String, String> producer;

    private String topic;

    /**
     * init producer.
     *
     * @param props kafka props
     */
    public void initProducer(final Properties props) {
        if (MapUtils.isEmpty(props)) {
            LOG.error("kafka props is empty. failed init kafka producer");
            return;
        }
        if (isStarted.get()) {
            close();
        }
        String topic = "shenyu-access-logging";
        String nameserverAddress = props.getProperty("bootstrap.servers");
        if (StringUtils.isBlank(topic) || StringUtils.isBlank(nameserverAddress)) {
            LOG.error("init kafkaLogCollectClient error, please check topic or nameserverAddress");
            return;
        }
        this.topic = topic;
        producer = new KafkaProducer<>(props);
        ProducerRecord<String, String> record = new ProducerRecord<>("shenyu-access-logging", StringSerializer.class.getName(), StringSerializer.class.getName());
        try {
            producer.send(record);
            LOG.info("init kafkaLogCollectClient success");
            isStarted.set(true);
        } catch (ProducerFencedException | OutOfOrderSequenceException | AuthorizationException e) {
            // We can't recover from these exceptions, so our only option is to close the producer and exit.
            LOG.error("Init kafkaLogCollectClient error, We can't recover from these exceptions, so our only option is to close the producer and exit", e);
            producer.close();
        } catch (KafkaException e) {
            // For all other exceptions, just abort the transaction and try again.
            LOG.error(
                "init kafkaLogCollectClient error，Exceptions other than ProducerFencedException or OutOfOrderSequenceException or AuthorizationException, just abort the transaction and try again", e);
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
                producer.send(toProducerRecord(logTopic, log));
            } catch (Exception e) {
                LOG.error("kafka push logs error", e);
            }
        });
    }

    private ProducerRecord<String, String> toProducerRecord(final String logTopic, final ShenyuRequestLog log) {
        byte[] bytes = JsonUtils.toJson(log).getBytes(StandardCharsets.UTF_8);
        String compressAlg = StringUtils.defaultIfBlank(KafkaLogCollectConfig.INSTANCE.getKafkaLogConfig().getCompressAlg(), "");
        if ("LZ4".equalsIgnoreCase(compressAlg.trim())) {
            LZ4CompressData lz4CompressData = new LZ4CompressData(bytes.length, compressedByte(bytes));
            return new ProducerRecord<>(logTopic, JsonUtils.toJson(lz4CompressData));

        } else {
            return new ProducerRecord<>(logTopic, JsonUtils.toJson(log));
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
            producer.close();
            isStarted.set(false);
        }
    }
}
