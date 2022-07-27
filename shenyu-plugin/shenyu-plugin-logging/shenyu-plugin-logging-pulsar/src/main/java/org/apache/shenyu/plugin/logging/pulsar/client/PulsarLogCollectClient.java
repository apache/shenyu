package org.apache.shenyu.plugin.logging.pulsar.client;

import net.jpountz.lz4.LZ4Compressor;
import net.jpountz.lz4.LZ4Factory;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.pulsar.client.api.Producer;
import org.apache.pulsar.client.api.PulsarClient;
import org.apache.pulsar.client.api.PulsarClientException;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.common.client.LogConsumeClient;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.LZ4CompressData;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.pulsar.config.PulsarLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

public class PulsarLogCollectClient implements LogConsumeClient {
    private static final Logger LOG = LoggerFactory.getLogger(PulsarLogCollectClient.class);

    private PulsarClient client;

    private Producer<byte[]> producer;

    private final AtomicBoolean isStarted = new AtomicBoolean(false);

    public void initProducer(final Properties props) {
        if (MapUtils.isEmpty(props)) {
            LOG.error("Pulsar props is empty. Fail to init Pulsar producer.");
            return;
        }
        if (isStarted.get()) {
            close();
        }
        String topic = props.getProperty(GenericLoggingConstant.TOPIC);
        String serviceUrl = props.getProperty(GenericLoggingConstant.SERVICE_URL);
        if (StringUtils.isBlank(topic) || StringUtils.isBlank(serviceUrl)) {
            LOG.error("init PulsarLogCollectClient error, please check topic or serviceUrl.");
            return;
        }
        try {
            client = PulsarClient.builder().serviceUrl(serviceUrl).build();
            producer = client.newProducer().topic(topic).create();
            LOG.info("init PulsarLogCollectClient success.");
            isStarted.set(true);
            Runtime.getRuntime().addShutdownHook(new Thread(this::close));

        } catch (PulsarClientException e) {
            LOG.error("init PulsarLogCollectClient error, ", e);
        }

    }

    @Override
    public void consume(final List<ShenyuRequestLog> logs) {
        if (CollectionUtils.isEmpty(logs) || !isStarted.get()) {
            return;
        }
        logs.forEach(log -> {
            producer.sendAsync(toBytes(log));
        });
    }

    private byte[] toBytes(ShenyuRequestLog log) {
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
    public void close() {
        if (Objects.nonNull(producer) && isStarted.get()) {
            try {
                producer.close();
                client.close();
                isStarted.set(false);
            } catch (PulsarClientException e) {
                LOG.error("fail to close PulsarLogCollectClient, e", e);
            }
        }
    }
}
