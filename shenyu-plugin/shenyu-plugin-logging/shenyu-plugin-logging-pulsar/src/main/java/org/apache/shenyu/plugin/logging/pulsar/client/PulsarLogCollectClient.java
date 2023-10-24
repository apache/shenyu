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

package org.apache.shenyu.plugin.logging.pulsar.client;

import net.jpountz.lz4.LZ4Compressor;
import net.jpountz.lz4.LZ4Factory;
import org.apache.commons.lang3.StringUtils;
import org.apache.pulsar.client.api.Producer;
import org.apache.pulsar.client.api.PulsarClient;
import org.apache.pulsar.client.api.PulsarClientException;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.entity.LZ4CompressData;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.pulsar.config.PulsarLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

/**
 * queue-based logging collector.
 */
public class PulsarLogCollectClient extends AbstractLogConsumeClient<PulsarLogCollectConfig.PulsarLogConfig, ShenyuRequestLog> {
    private static final Logger LOG = LoggerFactory.getLogger(PulsarLogCollectClient.class);

    private PulsarClient client;

    private Producer<byte[]> producer;

    /**
     * init producer.
     * 
     * @param config pulsar props
     */
    @Override
    public void initClient0(@NonNull final PulsarLogCollectConfig.PulsarLogConfig config) {
        String topic = config.getTopic();
        String serviceUrl = config.getServiceUrl();
        if (StringUtils.isBlank(topic) || StringUtils.isBlank(serviceUrl)) {
            LOG.error("init PulsarLogCollectClient error, please check topic or serviceUrl.");
            return;
        }
        try {
            client = PulsarClient.builder().serviceUrl(serviceUrl).build();
            producer = client.newProducer().topic(topic).create();
            LOG.info("init PulsarLogCollectClient success.");
            Runtime.getRuntime().addShutdownHook(new Thread(this::close));

        } catch (PulsarClientException e) {
            LOG.error("init PulsarLogCollectClient error, ", e);
        }
    }

    @Override
    public void consume0(@NonNull final List<ShenyuRequestLog> logs) {
        logs.forEach(log -> producer.sendAsync(toBytes(log)));
    }

    private byte[] toBytes(final ShenyuRequestLog log) {
        byte[] bytes = JsonUtils.toJson(log).getBytes(StandardCharsets.UTF_8);
        String compressAlg = StringUtils.defaultIfBlank(PulsarLogCollectConfig.INSTANCE.getPulsarLogConfig().getCompressAlg(), "");
        if ("LZ4".equalsIgnoreCase(compressAlg.trim())) {
            LZ4CompressData lz4CompressData = new LZ4CompressData(bytes.length, compressedByte(bytes));
            return JsonUtils.toJson(lz4CompressData).getBytes(StandardCharsets.UTF_8);
        } else {
            return bytes;
        }
    }

    private byte[] compressedByte(final byte[] srcByte) {
        LZ4Factory factory = LZ4Factory.fastestInstance();
        LZ4Compressor compressor = factory.fastCompressor();
        return compressor.compress(srcByte);
    }

    @Override
    public void close0() {
        if (Objects.nonNull(producer)) {
            try {
                producer.close();
                client.close();
            } catch (PulsarClientException e) {
                LOG.error("fail to close PulsarLogCollectClient, e", e);
            }
        }
    }
}
