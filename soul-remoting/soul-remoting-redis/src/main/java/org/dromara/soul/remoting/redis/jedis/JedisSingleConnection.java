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

import org.dromara.soul.remoting.redis.command.RedisKeyCommands;
import org.dromara.soul.remoting.redis.RedisModule;
import org.dromara.soul.remoting.redis.command.RedisStringsCommands;
import redis.clients.jedis.HostAndPort;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;
import redis.clients.jedis.util.Pool;

import java.util.Set;

/**
 * SinginConnection .
 * <p>
 * 2019/11/23
 *
 * @author sixh
 */
public class JedisSingleConnection extends JedisConnection {

    private Pool<Jedis> pool;


    public JedisSingleConnection(RedisModule module, JedisPoolConfig poolConfig,
                                 Set<HostAndPort> hostAndPorts) {
        super(poolConfig, hostAndPorts);
        this.pool = this.getPool(module);
    }

    public static JedisConnection build(RedisModule module, JedisPoolConfig config, Set<HostAndPort> hostAndPorts) {
        return new JedisSingleConnection(module, config, hostAndPorts);
    }

    public Pool<Jedis> getPool(RedisModule module) {
        HostAndPort host = getHosts().stream().findAny().orElse(null);
        if (host == null) {
            throw new NullPointerException("hostAndPort is null");
        }
        return new JedisPool(
                getPoolConfig(),
                host.getHost(),
                host.getPort(),
                module.getTimeOut(),
                module.getPassword()
        );
    }

    private Jedis getJedis() {
        return pool.getResource();
    }

    @SuppressWarnings("unchecked")
    <T> T execute(JedisExecuteCommand command) {
        Jedis jedis = getJedis();
        try {
            return (T) command.execute(jedis);
        } finally {
            jedis.close();
        }
    }

    @Override
    public RedisStringsCommands stringsCommands() {
        return new JedisStringsCommands(this);
    }

    @Override
    public RedisKeyCommands keyCommands() {
        return null;
    }
}
