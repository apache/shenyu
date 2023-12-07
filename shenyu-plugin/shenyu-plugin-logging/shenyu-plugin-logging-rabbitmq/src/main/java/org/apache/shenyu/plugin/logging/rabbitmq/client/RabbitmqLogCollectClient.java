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

package org.apache.shenyu.plugin.logging.rabbitmq.client;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.MessageProperties;
import net.jpountz.lz4.LZ4Compressor;
import net.jpountz.lz4.LZ4Factory;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.entity.LZ4CompressData;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.rabbitmq.config.RabbitmqLogCollectConfig;
import org.springframework.lang.NonNull;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeoutException;

/**
 * queue-based logging collector.
 */
public class RabbitmqLogCollectClient extends AbstractLogConsumeClient<RabbitmqLogCollectConfig.RabbitmqLogConfig, ShenyuRequestLog> {

    private String exchangeName;

    private Connection connection;

    private Channel channel;

    private String routingKey;

    @Override
    public void initClient0(@NonNull final RabbitmqLogCollectConfig.RabbitmqLogConfig config) {
        if (StringUtils.isBlank(config.getHost())
                || Objects.isNull(config.getPort())
                || StringUtils.isBlank(config.getExchangeName())
                || StringUtils.isBlank(config.getQueueName())
                || StringUtils.isBlank(config.getExchangeType())) {
            LOG.error("rabbitmq prop is empty. failed init rabbit producer");
            return;
        }

        String queueName = config.getQueueName();
        exchangeName = config.getExchangeName();
        routingKey = config.getRoutingKey();

        ConnectionFactory factory = new ConnectionFactory();

        factory.setVirtualHost(config.getVirtualHost());
        factory.setHost(config.getHost());
        factory.setPort(config.getPort());
        factory.setUsername(config.getUsername());
        factory.setPassword(config.getPassword());

        try {
            connection = factory.newConnection();
            channel = connection.createChannel();
            channel.exchangeDeclare(exchangeName, config.getExchangeType(), true);
            channel.queueDeclare(queueName, config.getDurable(), config.getExclusive(), config.getAutoDelete(), config.getArgs());
            channel.queueBind(queueName, exchangeName, routingKey);
            LOG.info("init rabbitmqLogCollectClient success");
        } catch (IOException e) {
            LOG.error("failed to initialize Rabbitmq connection", e);
        } catch (TimeoutException e) {
            LOG.error("failed to connect rabbitmq, connect timeout", e);
        }

    }

    @Override
    public void consume0(@NonNull final List<ShenyuRequestLog> logs) {
        logs.forEach(log -> {
            try {
                channel.basicPublish(exchangeName, routingKey, MessageProperties.PERSISTENT_TEXT_PLAIN, buildLogMessageBytes(log));
//                LOG.info("publish log message success:{}", log);
            } catch (Exception e) {
                LOG.error("rabbitmq push logs error", e);
            }
        });
    }

    /**
     * build the log msg bytes.
     *
     * @param log log
     * @return log msg bytes
     */
    private byte[] buildLogMessageBytes(final ShenyuRequestLog log) {
        byte[] bytes = JsonUtils.toJson(log).getBytes(StandardCharsets.UTF_8);
        LZ4CompressData lz4CompressData = new LZ4CompressData(bytes.length, compressedByte(bytes));
        bytes = JsonUtils.toJson(lz4CompressData).getBytes();
        return bytes;
    }

    private byte[] compressedByte(final byte[] srcByte) {
        LZ4Factory factory = LZ4Factory.fastestInstance();
        LZ4Compressor compressor = factory.fastCompressor();
        return compressor.compress(srcByte);
    }

    /**
     * close the connection and channel.
     */
    @Override
    public void close0() throws Exception {
        try {
            if (Objects.nonNull(channel)) {
                channel.close();
            }
            if (Objects.nonNull(connection)) {
                connection.close();
            }
            LOG.info("close RabbitMQ connection success");
        } catch (IOException e) {
            LOG.error("failed to close RabbitMQ connection", e);
        }
    }
}
