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

package org.apache.shenyu.infra.redis;

import org.apache.shenyu.common.enums.RedisModeEnum;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.data.redis.connection.RedisNode;

import java.time.Duration;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

import static org.mockito.Mockito.when;

/**
 * RedisConnectionFactoryTest.
 */
public class RedisConnectionFactoryTest {

    @Test
    public void redisConnectionFactoryTest() {
        final RedisConfigProperties redisConfigProperties = Mockito.mock(RedisConfigProperties.class);
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

    @Test
    public void parseRedisNodeValidInputs() throws Exception {
        RedisConnectionFactory factory = createFactoryWithDefaultUrl();
        Method parseMethod = RedisConnectionFactory.class.getDeclaredMethod("parseRedisNode", String.class);
        parseMethod.setAccessible(true);

        // IPv4 with custom port
        Assertions.assertAll(
                () -> {
                    RedisNode node = (RedisNode) parseMethod.invoke(factory, "localhost:6380");
                    Assertions.assertEquals("localhost", node.getHost());
                    Assertions.assertEquals(6380, node.getPort());
                },
                () -> {
                    RedisNode node = (RedisNode) parseMethod.invoke(factory, "[::1]:6381");
                    Assertions.assertEquals("::1", node.getHost());
                    Assertions.assertEquals(6381, node.getPort());
                },
                () -> {
                    RedisNode node = (RedisNode) parseMethod.invoke(factory, "[::1]");
                    Assertions.assertEquals("::1", node.getHost());
                    Assertions.assertEquals(6379, node.getPort());
                },
                () -> {
                    RedisNode node = (RedisNode) parseMethod.invoke(factory, "redis.internal");
                    Assertions.assertEquals("redis.internal", node.getHost());
                    Assertions.assertEquals(6379, node.getPort());
                }
        );
    }

    @Test
    public void parseRedisNodeInvalidInputs() throws Exception {
        RedisConnectionFactory factory = createFactoryWithDefaultUrl();
        Method parseMethod = RedisConnectionFactory.class.getDeclaredMethod("parseRedisNode", String.class);
        parseMethod.setAccessible(true);

        Assertions.assertAll(
                () -> assertInvalidNode(parseMethod, factory, ""),
                () -> assertInvalidNode(parseMethod, factory, "[::1]:"),
                () -> assertInvalidNode(parseMethod, factory, "localhost:70000"),
                () -> assertInvalidNode(parseMethod, factory, "[]:6379")
        );
    }

    private RedisConnectionFactory createFactoryWithDefaultUrl() {
        RedisConfigProperties redisConfigProperties = new RedisConfigProperties();
        redisConfigProperties.setUrl("localhost:6379");
        redisConfigProperties.setMode(RedisModeEnum.STANDALONE.getName());
        return new RedisConnectionFactory(redisConfigProperties);
    }

    private void assertInvalidNode(final Method parseMethod, final RedisConnectionFactory factory, final String url) {
        Assertions.assertThrows(IllegalArgumentException.class, () -> {
            try {
                parseMethod.invoke(factory, url);
            } catch (InvocationTargetException e) {
                // unwrap to the original IllegalArgumentException
                if (e.getCause() instanceof RuntimeException) {
                    throw (RuntimeException) e.getCause();
                }
                throw new RuntimeException(e.getCause());
            }
        });
    }
}
