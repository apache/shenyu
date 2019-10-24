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

package org.dromara.soul.plugins.limiter.lettuce;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import io.lettuce.core.RedisClient;
import io.lettuce.core.RedisURI;
import io.lettuce.core.ScriptOutputType;
import io.lettuce.core.api.sync.RedisScriptingCommands;
import io.lettuce.core.cluster.RedisClusterClient;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.plugins.limiter.config.RedisConfig;
import org.dromara.soul.plugins.limiter.redis.RedisClientSide;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * The type Lettuce.
 *
 * @author xiaoyu
 */
public class Lettuce implements RedisClientSide {

    private RedisScriptingCommands<String, String> redisScriptingCommands;

    /**
     * Instantiates a new Lettuce.
     */
    public Lettuce() {
        RedisConfig redisConfig = ConfigEnv.getInstance().getConfig(RedisConfig.class);
        if (redisConfig.getSentinel()) {
            List<RedisNode> redisNode = createRedisNode(redisConfig.getSentinelUrl());
            RedisURI redisURI = sentinelRedisURI(redisNode, redisConfig.getMasterName());
            toOptional(redisConfig.getPassword()).ifPresent(redisURI::setPassword);
            redisURI.setTimeout(Duration.ofMillis(redisConfig.getTimeOut()));
            redisScriptingCommands = RedisClient.create(redisURI).connect().sync();
        } else if (redisConfig.getCluster()) {
            List<RedisNode> redisNode = createRedisNode(redisConfig.getClusterUrl());
            List<RedisURI> initialUris = new ArrayList<>();
            for (RedisNode node : redisNode) {
                initialUris.add(createRedisUR(node, redisConfig.getPassword()));
            }
            redisScriptingCommands = RedisClusterClient.create(initialUris).connect().sync();
        } else {
            RedisURI redisURI = RedisURI.builder().withHost(redisConfig.getHost()).withPort(redisConfig.getPort()).build();
            toOptional(redisConfig.getPassword()).ifPresent(redisURI::setPassword);
            redisScriptingCommands = RedisClient.create(redisURI).connect().sync();
        }
    }


    @Override
    public Object evalsha(String script, List<String> keys, List<String> args) {
        return redisScriptingCommands.evalsha(script, ScriptOutputType.VALUE,
                keys.toArray(new String[0]) ,args.toArray(new String[0]));
    }

    private Optional<String> toOptional(String password) {
        if (StringUtils.isNotBlank(password)) {
            return Optional.of(password);
        }
        return Optional.empty();
    }

    private RedisURI createRedisUR(RedisNode redisNode, String password) {
        RedisURI.Builder builder = RedisURI.Builder.redis(redisNode.getHost(), redisNode.getPort());
        toOptional(password).ifPresent(builder::withPassword);
        return builder.build();
    }

    private RedisURI sentinelRedisURI(List<RedisNode> sentinels, String master) {
        RedisURI.Builder builder = null;
        for (RedisNode sentinel : sentinels) {
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

    private List<RedisNode> createRedisNode(final String url) {
        List<RedisNode> redisNodes = new ArrayList<>();
        List<String> nodes = Lists.newArrayList(Splitter.on(";").split(url));
        for (String node : nodes) {
            try {
                List<String> parts = Lists.newArrayList(Splitter.on(":").split(node));
                redisNodes.add(new RedisNode(parts.get(0), Integer.parseInt(parts.get(1))));
            } catch (RuntimeException ex) {
                throw new IllegalStateException(
                        "Invalid redis sentinel " + "property '" + node + "'", ex);
            }
        }
        return redisNodes;
    }


}
