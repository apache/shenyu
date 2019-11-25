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

import java.util.Set;
import java.util.stream.Collectors;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.remoting.redis.RedisModule;
import org.dromara.soul.remoting.redis.RemotingRedisConst;
import org.dromara.soul.remoting.redis.operation.*;
import redis.clients.jedis.HostAndPort;
import redis.clients.jedis.JedisPoolConfig;

/**
 * JedisClient .
 * <p>
 * 2019/11/23
 *
 * @author sixh
 */
class JedisOperation<K,V> implements RedisOperation<K,V> {

    private JedisConnection connection;

    public JedisOperation(RedisModule module) {
        this.connection = initJedisClient(module);
    }

    private JedisConnection initJedisClient(final RedisModule module) {
        JedisPoolConfig config = new JedisPoolConfig();
        config.setMaxIdle(module.getMaxIdle());
        config.setMinIdle(module.getMinIdle());
        config.setMaxTotal(module.getMaxTotal());
        config.setMaxWaitMillis(module.getMaxWaitMillis());
        config.setTestOnBorrow(module.getTestOnBorrow());
        config.setTestOnReturn(module.getTestOnReturn());
        config.setTestWhileIdle(module.getTestWhileIdle());
        config.setMinEvictableIdleTimeMillis(module.getMinEvictableIdleTimeMillis());
        config.setSoftMinEvictableIdleTimeMillis(module.getSoftMinEvictableIdleTimeMillis());
        config.setTimeBetweenEvictionRunsMillis(module.getTimeBetweenEvictionRunsMillis());
        config.setNumTestsPerEvictionRun(module.getNumTestsPerEvictionRun());
        Set<HostAndPort> hosts = getHost(module.getHosts());
        if (RemotingRedisConst.MODE_CLUSTER.equals(module.getMode())) {
            return JedisClusterConnection.build(config, hosts);
        } else if (RemotingRedisConst.MODE_SENTINEL.equals(module.getMode())) {
            return JedisSentinelConnection.build(module, config, hosts);
        } else if (RemotingRedisConst.MODE_DEFAULT.equals(module.getMode())) {
            return JedisSingleConnection.build(module, config, hosts);
        } else {
            throw new SoulException("redis mode unknown, Need is one of them(default, cluster, sentinel)");
        }
    }

    private Set<HostAndPort> getHost(Set<String> hosts) {
        return hosts.stream()
                .map(HostAndPort::parseString).collect(Collectors.toSet());
    }

    public JedisConnection getConnection() {
        return connection;
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
        return new DefaultValueOperation<>(this.getConnection());
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
