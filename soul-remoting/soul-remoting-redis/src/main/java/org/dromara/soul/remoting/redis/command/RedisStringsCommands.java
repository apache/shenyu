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

package org.dromara.soul.remoting.redis.command;

import java.util.concurrent.TimeUnit;

/**
 * Redis .
 * redis string commands.
 *
 * @author sixh
 * @see <a href="#"> strings commands</>
 */
public interface RedisStringsCommands {
    /**
     * get command.
     *
     * @param key key.
     * @return value. byte [ ]
     * @see <a href="#"> strings get command</>
     */
    byte[] get(byte[] key);

    /**
     * set command.
     *
     * @param key   the key
     * @param value the value
     */
    void set(byte[] key, byte[] value);

    /**
     * Set.
     *
     * @param key   the key
     * @param value the value
     * @param ttl   the ttl
     * @param unit  the unit
     */
    void set(byte[] key, byte[] value, long ttl, TimeUnit unit);
}
