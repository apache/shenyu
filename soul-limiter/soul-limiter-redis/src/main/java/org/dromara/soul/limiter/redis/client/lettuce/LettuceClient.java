/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.limiter.redis.client.lettuce;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import io.lettuce.core.RedisClient;
import io.lettuce.core.RedisURI;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.cluster.RedisClusterClient;
import io.lettuce.core.cluster.api.StatefulRedisClusterConnection;
import io.lettuce.core.resource.ClientResources;
import io.lettuce.core.support.ConnectionPoolSupport;
import org.apache.commons.pool2.impl.GenericObjectPool;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.limiter.redis.client.RedisClientSide;
import org.dromara.soul.limiter.redis.config.RedisConfig;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * The type Lettuce.
 *
 * @author xiaoyu
 */
public class LettuceClient implements RedisClientSide {

    private LettuceTypeClient lettuceTypeClient;

    /**
     * Instantiates a new Lettuce.
     */
    public LettuceClient() {
        RedisConfig redisConfig = ConfigEnv.getInstance().getConfig(RedisConfig.class);
        GenericObjectPoolConfig poolConfig = new GenericObjectPoolConfig();
        if (redisConfig.getSentinel()) {
            List<LettuceRedisNode> lettuceRedisNode = createRedisNode(redisConfig.getSentinelUrl());
            RedisURI redisURI = sentinelRedisURI(lettuceRedisNode, redisConfig.getMasterName());
            toOptional(redisConfig.getPassword()).ifPresent(redisURI::setPassword);
            RedisClient redisClient = RedisClient.create(ClientResources.create(), redisURI);
            GenericObjectPool<StatefulRedisConnection<String, String>> pool
                    = ConnectionPoolSupport.createGenericObjectPool(redisClient::connect, poolConfig);
            lettuceTypeClient = new LettuceSentinelTypeClient(pool);
        } else if (redisConfig.getCluster()) {
            List<LettuceRedisNode> lettuceRedisNode = createRedisNode(redisConfig.getClusterUrl());
            List<RedisURI> initialUris = new ArrayList<>();
            for (LettuceRedisNode node : lettuceRedisNode) {
                initialUris.add(createRedisUR(node, redisConfig.getPassword()));
            }
            RedisClusterClient redisClusterClient = RedisClusterClient.create(ClientResources.create(), initialUris);
            GenericObjectPool<StatefulRedisClusterConnection<String, String>> pool
                    = ConnectionPoolSupport.createGenericObjectPool(redisClusterClient::connect, poolConfig);
            lettuceTypeClient = new LettuceClusterTypeClient(pool);

        } else {
            RedisURI redisURI = RedisURI.builder().withHost(redisConfig.getHost()).withPort(redisConfig.getPort()).build();
            toOptional(redisConfig.getPassword()).ifPresent(redisURI::setPassword);
            RedisClient redisClient = RedisClient.create(ClientResources.create(), redisURI);
            GenericObjectPool<StatefulRedisConnection<String, String>> pool
                    = ConnectionPoolSupport.createGenericObjectPool(redisClient::connect, poolConfig);
            lettuceTypeClient = new LettuceSentinelTypeClient(pool);
        }
    }

    @Override
    public Object evalsha(String script, List<String> keys, List<String> args) {
        return lettuceTypeClient.evalsha(script, keys, args);

    }

    private Optional<String> toOptional(String password) {
        if (StringUtils.isNotBlank(password)) {
            return Optional.of(password);
        }
        return Optional.empty();
    }

    private RedisURI createRedisUR(LettuceRedisNode lettuceRedisNode, String password) {
        RedisURI.Builder builder = RedisURI.Builder.redis(lettuceRedisNode.getHost(), lettuceRedisNode.getPort());
        toOptional(password).ifPresent(builder::withPassword);
        return builder.build();
    }

    private RedisURI sentinelRedisURI(List<LettuceRedisNode> sentinels, String master) {
        RedisURI.Builder builder = null;
        for (LettuceRedisNode sentinel : sentinels) {
            if (builder == null) {
                builder = RedisURI.Builder.sentinel(sentinel.getHost(), sentinel.getPort(),
                        master);
            } else {
                builder.withSentinel(sentinel.getHost(), sentinel.getPort());
            }
        }
        assert builder != null;
        return builder.build();
    }

    private List<LettuceRedisNode> createRedisNode(final String url) {
        List<LettuceRedisNode> lettuceRedisNodes = new ArrayList<>();
        List<String> nodes = Lists.newArrayList(Splitter.on(";").split(url));
        for (String node : nodes) {
            try {
                List<String> parts = Lists.newArrayList(Splitter.on(":").split(node));
                lettuceRedisNodes.add(new LettuceRedisNode(parts.get(0), Integer.parseInt(parts.get(1))));
            } catch (RuntimeException ex) {
                throw new IllegalStateException(
                        "Invalid redis sentinel " + "property '" + node + "'", ex);
            }
        }
        return lettuceRedisNodes;
    }


}
