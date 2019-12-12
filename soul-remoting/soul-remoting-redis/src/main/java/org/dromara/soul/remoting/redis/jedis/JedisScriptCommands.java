/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.remoting.redis.jedis;

import java.util.List;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.remoting.redis.command.RedisScriptCommands;

/**
 * JedisScriptCommands
 * jedis implementation redis script command.
 *
 * @author sixh
 */
public class JedisScriptCommands implements RedisScriptCommands {

    private JedisSingleConnection connection;

    public JedisScriptCommands(JedisSingleConnection connection) {
        this.connection = connection;
    }

    @Override
    public Object evalSha(byte[] script, List<byte[]> keys, List<byte[]> args) {
        try {
            return connection.execute(jedis -> jedis.evalsha(script, keys, args));
        } catch (RuntimeException ex) {
            throw new SoulException(ex);
        }
    }

    @Override
    public Object eval(byte[] script, List<byte[]> keys, List<byte[]> args) {
        try {
            return connection.execute(jedis -> jedis.eval(script, keys, args));
        } catch (RuntimeException ex) {
            throw new SoulException(ex);
        }
    }
}
