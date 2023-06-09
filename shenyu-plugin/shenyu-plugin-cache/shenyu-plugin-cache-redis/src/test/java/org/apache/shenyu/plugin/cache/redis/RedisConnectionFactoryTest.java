/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.cache.redis;

import org.apache.shenyu.common.enums.RedisModeEnum;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.time.Duration;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * RedisConnectionFactoryTest.
 */
public class RedisConnectionFactoryTest {

    @Test
    public void redisConnectionFactoryTest() {
        final RedisConfigProperties redisConfigProperties = mock(RedisConfigProperties.class);
        when(redisConfigProperties.getMode()).thenReturn(RedisModeEnum.SENTINEL.getName());
        when(redisConfigProperties.getMaster()).thenReturn("master");
        when(redisConfigProperties.getUrl()).thenReturn("localhost:6379");
        Assertions.assertDoesNotThrow(() -> new RedisConnectionFactory(redisConfigProperties));
        when(redisConfigProperties.getPassword()).thenReturn("password");
        Assertions.assertDoesNotThrow(() -> new RedisConnectionFactory(redisConfigProperties));
        when(redisConfigProperties.getMode()).thenReturn(RedisModeEnum.CLUSTER.getName());
        Assertions.assertDoesNotThrow(() -> new RedisConnectionFactory(redisConfigProperties));
        when(redisConfigProperties.getPassword()).thenReturn(null);
        Assertions.assertDoesNotThrow(() -> new RedisConnectionFactory(redisConfigProperties));
        when(redisConfigProperties.getMode()).thenReturn(RedisModeEnum.STANDALONE.getName());
        when(redisConfigProperties.getPassword()).thenReturn("password");
        when(redisConfigProperties.getMaxWait()).thenReturn(Duration.ofMillis(-1));
        Assertions.assertDoesNotThrow(() -> new RedisConnectionFactory(redisConfigProperties));
    }
}
