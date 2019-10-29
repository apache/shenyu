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

package org.dromara.soul.limiter.redis.client.jedis;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.limiter.redis.client.RedisClientSide;
import org.dromara.soul.limiter.redis.config.RedisConfig;
import redis.clients.jedis.HostAndPort;
import redis.clients.jedis.JedisCluster;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;
import redis.clients.jedis.JedisSentinelPool;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The type Jedis.
 *
 * @author xiaoyu
 */
public class JedisClient implements RedisClientSide {

    private JedisTypeClient jedisTypeClient;

    /**
     * Instantiates a new Jedis.
     */
    public JedisClient() {
        RedisConfig redisConfig = ConfigEnv.getInstance().getConfig(RedisConfig.class);
        jedisTypeClient = initJedisClient(redisConfig);
    }

    @Override
    public Object evalsha(String script, List<String> keys, List<String> args) {
        return jedisTypeClient.evalsha(script, keys, args);
    }

    private JedisTypeClient initJedisClient(final RedisConfig redisConfig) {
        JedisPoolConfig config = new JedisPoolConfig();
        config.setMaxIdle(redisConfig.getMaxIdle());
        config.setMinIdle(redisConfig.getMinIdle());
        config.setMaxTotal(redisConfig.getMaxTotal());
        config.setMaxWaitMillis(redisConfig.getMaxWaitMillis());
        config.setTestOnBorrow(redisConfig.getTestOnBorrow());
        config.setTestOnReturn(redisConfig.getTestOnReturn());
        config.setTestWhileIdle(redisConfig.getTestWhileIdle());
        config.setMinEvictableIdleTimeMillis(redisConfig.getMinEvictableIdleTimeMillis());
        config.setSoftMinEvictableIdleTimeMillis(redisConfig.getSoftMinEvictableIdleTimeMillis());
        config.setTimeBetweenEvictionRunsMillis(redisConfig.getTimeBetweenEvictionRunsMillis());
        config.setNumTestsPerEvictionRun(redisConfig.getNumTestsPerEvictionRun());
        JedisPool jedisPool;
        if (redisConfig.getCluster()) {
            final String clusterUrl = redisConfig.getClusterUrl();
            final Set<HostAndPort> hostAndPorts =
                    Lists.newArrayList(Splitter.on(";").trimResults().split(clusterUrl))
                            .stream()
                            .map(HostAndPort::parseString).collect(Collectors.toSet());
            JedisCluster jedisCluster = new JedisCluster(hostAndPorts, config);
            return new JedisClusterTypeClient(jedisCluster);
        } else if (redisConfig.getSentinel()) {
            final String sentinelUrl = redisConfig.getSentinelUrl();
            final Set<String> hostAndPorts =
                    new HashSet<>(Lists.newArrayList(Splitter.on(";").split(sentinelUrl)));
            JedisSentinelPool pool =
                    new JedisSentinelPool(redisConfig.getMasterName(), hostAndPorts,
                            config, redisConfig.getTimeOut(), redisConfig.getPassword());
            return new JedisSentinelTypeClient(pool);
        } else {
            if (StringUtils.isNoneBlank(redisConfig.getPassword())) {
                jedisPool = new JedisPool(config, redisConfig.getHost(), redisConfig.getPort(), redisConfig.getTimeOut(), redisConfig.getPassword());
            } else {
                jedisPool = new JedisPool(config, redisConfig.getHost(), redisConfig.getPort(), redisConfig.getTimeOut());
            }
            return new JedisSingleTypeClient(jedisPool);
        }
    }
}
