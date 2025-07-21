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

package org.apache.shenyu.plugin.logging.rabbitmq.cache;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.logging.rabbitmq.client.RabbitmqLogCollectClient;
import org.apache.shenyu.plugin.logging.rabbitmq.config.RabbitmqLogCollectConfig;

import java.util.Map;
import java.util.Objects;

import static org.apache.shenyu.plugin.api.ShenyuPlugin.LOG;

/**
 * rabbitmq client cache.
 */
public class RabbitmqClientCache {

    private static final Map<String, RabbitmqLogCollectClient> CLIENT_CACHE = Maps.newConcurrentMap();

    public RabbitmqClientCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static RabbitmqClientCache getInstance() {
        return ApplicationConfigCacheInstance.INSTANCE;
    }

    /**
     * Init client.
     *
     * @param selectorId selectorId
     * @param rabbitmqLogUpstream rabbitmqLogUpstream
     */
    public void initRabbitmqClient(final String selectorId, final RabbitmqLogCollectConfig.LogApiConfig rabbitmqLogUpstream) {
        RabbitmqLogCollectConfig.RabbitmqLogConfig globalLogConfig = Singleton.INST.get(RabbitmqLogCollectConfig.RabbitmqLogConfig.class);
        if (Objects.nonNull(rabbitmqLogUpstream) && Objects.nonNull(rabbitmqLogUpstream.getHost())) {
            globalLogConfig = copyConfig(rabbitmqLogUpstream);
        }
        RabbitmqLogCollectClient rabbitmqLogCollectClient = new RabbitmqLogCollectClient();
        rabbitmqLogCollectClient.initClient(globalLogConfig);
        CLIENT_CACHE.put(selectorId, rabbitmqLogCollectClient);
    }

    /**
     * Get the client.
     *
     * @param path path
     * @return Channel channel
     */
    public RabbitmqLogCollectClient getRabbitmqClient(final String path) {
        return CLIENT_CACHE.get(path);
    }

    /**
     * getClientCache.
     *
     * @return clientCache
     */
    public Map<String, RabbitmqLogCollectClient> getClientCache() {
        return CLIENT_CACHE;
    }

    /**
     * invalidate the client by selectorId.
     *
     * @param path path
     */
    public void invalidate(final String path) {
        RabbitmqLogCollectClient client = CLIENT_CACHE.get(path);
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

    public static RabbitmqLogCollectConfig.RabbitmqLogConfig copyConfig(final RabbitmqLogCollectConfig.LogApiConfig rabbitmqLogUpstream) {
        RabbitmqLogCollectConfig.RabbitmqLogConfig rabbitmqLogConfig = new RabbitmqLogCollectConfig.RabbitmqLogConfig();
        rabbitmqLogConfig.setQueueName(rabbitmqLogUpstream.getQueueName());
        rabbitmqLogConfig.setHost(rabbitmqLogUpstream.getHost());
        rabbitmqLogConfig.setPort(rabbitmqLogUpstream.getPort());
        rabbitmqLogConfig.setUsername(rabbitmqLogUpstream.getUsername());
        rabbitmqLogConfig.setPassword(rabbitmqLogUpstream.getPassword());
        rabbitmqLogConfig.setRoutingKey(rabbitmqLogUpstream.getRoutingKey());
        rabbitmqLogConfig.setExchangeName(rabbitmqLogUpstream.getExchangeName());
        rabbitmqLogConfig.setExchangeType(rabbitmqLogUpstream.getExchangeType());
        rabbitmqLogConfig.setVirtualHost(rabbitmqLogUpstream.getVirtualHost());
        rabbitmqLogConfig.setExclusive(rabbitmqLogUpstream.getExclusive());
        rabbitmqLogConfig.setDurable(rabbitmqLogUpstream.getDurable());
        rabbitmqLogConfig.setAutoDelete(rabbitmqLogUpstream.getAutoDelete());
        rabbitmqLogConfig.setArgs(rabbitmqLogUpstream.getArgs());
        return rabbitmqLogConfig;
    }

    /**
     * The type Application config cache instance.
     */
    static final class ApplicationConfigCacheInstance {

        /**
         * The Instance.
         */
        static final RabbitmqClientCache INSTANCE = new RabbitmqClientCache();

        private ApplicationConfigCacheInstance() {

        }
    }
}
