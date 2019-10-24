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

package org.dromara.soul.plugins.limiter.jedis;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisSentinelPool;

import java.util.List;
import java.util.Set;

/**
 * JedisClientSentinel.
 *
 * @author xiaoyu(Myth)
 */
public class JedisClientSentinel implements JedisClient {

    private JedisSentinelPool jedisSentinelPool;

    JedisClientSentinel(final JedisSentinelPool jedisSentinelPool) {
        this.jedisSentinelPool = jedisSentinelPool;
    }

    @Override
    public Object evalsha(String sha1, List<String> keys, List<String> args) {
        try (Jedis jedis = jedisSentinelPool.getResource()) {
            return jedis.eval(sha1, keys, args);
        }
    }

}
