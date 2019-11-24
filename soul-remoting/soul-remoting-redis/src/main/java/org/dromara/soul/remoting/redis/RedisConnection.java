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

import org.dromara.soul.remoting.redis.command.RedisCommands;
import org.dromara.soul.remoting.redis.command.RedisKeyCommands;
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



    /**
     * string commands.
     *
     * @return commands.
     */
    public abstract RedisStringsCommands stringsCommands();

    /**
     * keys commands.
     *
     * @return commands.
     */
    public abstract RedisKeyCommands keyCommands();
}
