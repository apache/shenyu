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

package org.dromara.soul.remoting.redis;

import java.util.List;
import java.util.concurrent.TimeUnit;
import org.dromara.soul.remoting.redis.command.RedisCommands;
import org.dromara.soul.remoting.redis.command.RedisKeyCommands;
import org.dromara.soul.remoting.redis.command.RedisScriptCommands;
import org.dromara.soul.remoting.redis.command.RedisStringsCommands;

/**
 * RedisConnection .
 *
 * @author sixh
 */
public abstract class RedisConnection implements RedisCommands {

    @Override
    public byte[] get(byte[] key) {
        return stringsCommands().get(key);
    }

    @Override
    public void set(byte[] key, byte[] value) {
        stringsCommands().set(key, value);
    }

    @Override
    public void setEx(byte[] key, byte[] value, long ttl, TimeUnit unit) {
        stringsCommands().setEx(key, value, ttl, unit);
    }

    @Override
    public void setPx(byte[] key, byte[] value, long ttl, TimeUnit unit) {
        stringsCommands().setPx(key, value, ttl, unit);
    }

    @Override
    public void setNx(byte[] key, byte[] value) {
        stringsCommands().setNx(key, value);
    }

    @Override
    public void setXx(byte[] key, byte[] value) {
        stringsCommands().setXx(key, value);
    }

    @Override
    public void append(byte[] key, byte[] value) {
        stringsCommands().append(key, value);
    }

    @Override
    public byte[] getAndSet(byte[] key, byte[] value) {
        return stringsCommands().getAndSet(key, value);
    }

    @Override
    public Object evalSha(byte[] script, List<byte[]> keys, List<byte[]> args) {
        return scriptCommands().evalSha(script, keys, args);
    }

    @Override
    public Object eval(byte[] script, List<byte[]> keys, List<byte[]> args) {
        return scriptCommands().eval(script, keys, args);
    }

    /**
     * string commands.
     *
     * @return commands. redis strings commands.
     */
    protected abstract RedisStringsCommands stringsCommands();

    /**
     * keys commands.
     *
     * @return commands. redis key commands.
     */
    protected abstract RedisKeyCommands keyCommands();

    /**
     * Script commands redis script commands.
     *
     * @return the redis script commands.
     */
    protected abstract RedisScriptCommands scriptCommands();
}
