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
import org.dromara.soul.remoting.redis.command.RedisScriptCommands;
import org.dromara.soul.remoting.redis.command.RedisStringsCommands;
import redis.clients.jedis.HostAndPort;
import redis.clients.jedis.JedisCluster;
import redis.clients.jedis.JedisPoolConfig;

import java.util.Set;

/**
 * JedisClusterConnection .
 * <p>
 * 2019/11/23
 *
 * @author sixh
 */
public class JedisClusterConnection extends JedisConnection {


    private JedisCluster jedisCluster;

    public JedisClusterConnection(JedisPoolConfig config, Set<HostAndPort> hostAndPorts) {
        super(config, hostAndPorts);
        this.jedisCluster = new JedisCluster(hostAndPorts, config);
    }

    public static JedisConnection build(JedisPoolConfig config, Set<HostAndPort> hostAndPorts) {
        return new JedisClusterConnection(config, hostAndPorts);
    }

    @SuppressWarnings("unchecked")
    public <T> T execute(JedisClusterExecuteCommand command) {
        return (T) command.execute(jedisCluster);
    }

    @Override
    public byte[] get(byte[] key) {
        return stringsCommands().get(key);
    }

    @Override
    public void set(byte[] key, byte[] value) {
        stringsCommands().set(key, value);
    }

    @Override
    protected RedisScriptCommands scriptCommands() {
        return null;
    }

    @Override
    public RedisStringsCommands stringsCommands() {
        return null;
    }

    @Override
    public RedisKeyCommands keyCommands() {
        return null;
    }
}
