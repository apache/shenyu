/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.dromara.soul.remoting.redis.jedis;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import org.dromara.soul.remoting.redis.*;
import org.dromara.soul.remoting.redis.operation.*;
import redis.clients.jedis.HostAndPort;
import redis.clients.jedis.JedisPoolConfig;

import java.util.Set;
import java.util.stream.Collectors;

/**
 * JedisClient .
 * <p>
 * 2019/11/23
 *
 * @author sixh
 */
class JedisOperation implements RedisOperation {

    private JedisConnection connection;

    public JedisOperation(RedisModule module) {
        initJedisClient(module);
    }

    private RedisConnection initJedisClient(final RedisModule redisConfig) {
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
        Set<HostAndPort> hosts = getHost(redisConfig.getHosts());
        if (redisConfig.getCluster()) {
            return JedisClusterConnection.build(config, hosts);
        } else if (redisConfig.getSentinel()) {
            return JedisSentinelConnection.build(redisConfig, config, hosts);
        } else {
            return JedisSingleConnection.build(redisConfig, config, hosts);
        }
    }

    Set<HostAndPort> getHost(String hostsStr) {
        return
                Lists.newArrayList(Splitter.on(",").trimResults().split(hostsStr))
                        .stream()
                        .map(HostAndPort::parseString).collect(Collectors.toSet());
    }

    @Override
    public MapOperation mapOperation() {
        return null;
    }

    @Override
    public ListOperation listOperation() {
        return null;
    }

    @Override
    public SetOperation setOperation() {
        return null;
    }

    @Override
    public ValueOperation valueOperation() {
        return null;
    }

    @Override
    public ZsetOperation zsetOperation() {
        return null;
    }

    @Override
    public ScriptOperation scriptOperation() {
        return null;
    }
}
