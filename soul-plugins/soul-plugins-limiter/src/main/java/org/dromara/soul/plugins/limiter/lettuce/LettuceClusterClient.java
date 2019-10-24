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

package org.dromara.soul.plugins.limiter.lettuce;

import io.lettuce.core.ScriptOutputType;
import io.lettuce.core.cluster.api.StatefulRedisClusterConnection;
import io.lettuce.core.cluster.api.sync.RedisAdvancedClusterCommands;
import org.apache.commons.pool2.impl.GenericObjectPool;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * The type Lettuce cluster client.
 *
 * @author xiaoyu
 */
public class LettuceClusterClient implements LettuceClient {

    private static final Logger LOGGER = LoggerFactory.getLogger(LettuceSentinelClient.class);

    private GenericObjectPool<StatefulRedisClusterConnection<String, String>> pool;

    /**
     * Instantiates a new Lettuce cluster client.
     *
     * @param pool the pool
     */
    LettuceClusterClient(GenericObjectPool<StatefulRedisClusterConnection<String, String>> pool) {
        this.pool = pool;
    }

    @Override
    public Object evalsha(String script, List<String> keys, List<String> args) {
        try (StatefulRedisClusterConnection<String, String> connection = pool.borrowObject()) {
            RedisAdvancedClusterCommands<String, String> command = connection.sync();
            return command.evalsha(script, ScriptOutputType.VALUE,
                    keys.toArray(new String[0]), args.toArray(new String[0]));
        } catch (Exception e) {
            LOGGER.error("LettuceClusterClient evalsha have exeception: ", e);
            return null;
        }
    }
}
