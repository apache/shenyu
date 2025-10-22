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

package org.apache.shenyu.plugin.logging.kafka.cache;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;


import java.util.Map;
import java.util.Objects;

import static org.apache.shenyu.plugin.api.ShenyuPlugin.LOG;

/**
 * KafkaClientCache.
 */
public class KafkaClientCache {
    private static final Map<String, KafkaLogCollectClient> CLIENT_CACHE = Maps.newConcurrentMap();

    public KafkaClientCache() {
    }

    public static KafkaClientCache getInstance() {
        return ApplicationConfigCacheInstance.INSTANCE;
    }

    public void initKafkaClient(final String selectorId, final KafkaLogCollectConfig.LogApiConfig kafkaUpstream) {
        KafkaLogCollectConfig.KafkaLogConfig globalLogConfig = Singleton.INST.get(KafkaLogCollectConfig.KafkaLogConfig.class);
        if (Objects.nonNull(kafkaUpstream) && Objects.nonNull(kafkaUpstream.getBootstrapServer())) {
            globalLogConfig = copyConfig(kafkaUpstream);
        }
        KafkaLogCollectClient kafkaLogCollectClient = new KafkaLogCollectClient();
        kafkaLogCollectClient.initClient(globalLogConfig);
        CLIENT_CACHE.put(selectorId, kafkaLogCollectClient);
    }

    public KafkaLogCollectClient getKafkaClient(final String selectorId) {
        return CLIENT_CACHE.get(selectorId);
    }

    /**
     * getClientCache.
     *
     * @return clientCache
     */
    public Map<String, KafkaLogCollectClient> getClientCache() {
        return CLIENT_CACHE;
    }

    /**
     * invalidate the client by selectorId.
     *
     * @param path path
     */
    public void invalidate(final String path) {
        KafkaLogCollectClient client = CLIENT_CACHE.get(path);
        if (Objects.nonNull(client)) {
            client.close();
            CLIENT_CACHE.remove(path);
        }
    }

    /**
     * invalidate all client.
     */
    public void invalidateAll() {
        if (CLIENT_CACHE.isEmpty()) {
            return;
        }

        CLIENT_CACHE.values().forEach(client -> {
            try {
                if (Objects.nonNull(client)) {
                    client.close();
                }
            } catch (Exception e) {
                LOG.error("Failed to close client {}", client, e);
            }
        });

        CLIENT_CACHE.clear();
    }

    public static KafkaLogCollectConfig.KafkaLogConfig copyConfig(final KafkaLogCollectConfig.LogApiConfig rabbitmqLogUpstream) {
        KafkaLogCollectConfig.KafkaLogConfig kafkaLogConfig = new KafkaLogCollectConfig.KafkaLogConfig();
        kafkaLogConfig.setTopic(rabbitmqLogUpstream.getTopic());
        kafkaLogConfig.setBootstrapServer(rabbitmqLogUpstream.getBootstrapServer());
        kafkaLogConfig.setProducerGroup(rabbitmqLogUpstream.getProducerGroup());
        kafkaLogConfig.setCompressAlg(rabbitmqLogUpstream.getCompressAlg());
        kafkaLogConfig.setSecurityProtocol(rabbitmqLogUpstream.getSecurityProtocol());
        kafkaLogConfig.setSaslMechanism(rabbitmqLogUpstream.getSaslMechanism());
        kafkaLogConfig.setUserName(rabbitmqLogUpstream.getUserName());
        kafkaLogConfig.setPassWord(rabbitmqLogUpstream.getPassWord());
        return kafkaLogConfig;
    }

    static final class ApplicationConfigCacheInstance {

        static final KafkaClientCache INSTANCE = new KafkaClientCache();

        private ApplicationConfigCacheInstance() {

        }
    }
}
