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
 * @see <a href="https://redis.io/commands#string"> strings commands</>
 */
public interface RedisStringsCommands {
    /**
     * get command.
     *
     * @param key key.
     * @return value. byte [ ]
     * @see <a href="https://redis.io/commands/get"> strings get command</>
     */
    byte[] get(byte[] key);

    /**
     * set command.
     *
     * @param key   the key
     * @param value the value.
     * @see <a href="https://redis.io/commands/set"> strings set command</>
     */
    void set(byte[] key, byte[] value);

    /**
     * Set.
     *
     * @param key   the key
     * @param value the value
     * @param ttl   the ttl
     * @param unit  the unit
     * @see <a href="https://redis.io/commands/set ex"> strings set command</>
     */
    void setEx(byte[] key, byte[] value, long ttl, TimeUnit unit);

    /**
     * Set px.
     *
     * @param key   the key
     * @param value the value
     * @param ttl   the ttl
     * @param unit  the unit
     * @see <a href="https://redis.io/commands/set px"> strings set command</>
     */
    void setPx(byte[] key, byte[] value, long ttl, TimeUnit unit);

    /**
     * Sets nx.
     *
     * @param key   the key
     * @param value the value
     * @see <a href="https://redis.io/commands/set nx"> strings set command</>
     */
    void setNx(byte[] key, byte[] value);

    /**
     * Sets xx.
     *
     * @param key   the key
     * @param value the value
     * @see <a href="https://redis.io/commands/set xx"> strings set command</>
     */
    void setXx(byte[] key, byte[] value);

    /**
     * append.
     *
     * @param key   the key
     * @param value the value
     * @see <a href="https://redis.io/commands/append"> strings set command</>
     */
    void append(byte[] key, byte[] value);

    /**
     * Get and set byte [ ].
     *
     * @param key   the key
     * @param value the value
     * @return the byte [ ]
     * @see <a href="https://redis.io/commands/getset"> strings set command</>
     */
    byte[] getAndSet(byte[] key, byte[] value);
}
