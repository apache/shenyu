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

package org.apache.shenyu.plugin.ratelimiter.handler;

import com.google.common.collect.Sets;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.cache.redis.RedisConfigProperties;
import org.apache.shenyu.plugin.cache.redis.RedisConnectionFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.redis.connection.RedisClusterConfiguration;
import org.springframework.data.redis.connection.RedisNode;
import org.springframework.data.redis.connection.RedisPassword;
import org.springframework.data.redis.connection.RedisSentinelConfiguration;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * RateLimiterPluginDataHandler test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class RateLimiterPluginDataHandlerTest {

    private static final String LOCALHOST = "localhost";

    private static final String PASSWORD_TEST_VALUE = "password";

    private static final String MASTER_TEST_VALUE = "master";

    private static final int DATABASE_TEST_VALUE = 1;

    private static final int PORT_TEST_VALUE_1 = 2181;

    private static final int PORT_TEST_VALUE_2 = 2182;

    private static final int DEFAULT_MAX_IDLE = 8;

    private static final int DEFAULT_MAX_ACTIVE = 8;

    private static final int DEFAULT_MIN_IDLE = 0;

    private RedisConnectionFactory redisConnectionFactory;

    @BeforeEach
    public void setUp() {
        this.redisConnectionFactory = new RedisConnectionFactory(generateRedisConfig(generateDefaultUrl()));
    }

    /**
     * handlerPlugin Singleton.INST init test case.
     */
    @Test
    public void handlerPluginTest() {
        RedisConfigProperties redisConfigProperties = generateRedisConfig(generateDefaultUrl());
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig(GsonUtils.getInstance().toJson(redisConfigProperties));
        new RateLimiterPluginDataHandler().handlerPlugin(pluginData);
        assertEquals(redisConfigProperties.getUrl(), Singleton.INST.get(RedisConfigProperties.class).getUrl());
        assertNotNull(Singleton.INST.get(ReactiveRedisTemplate.class));
    }

    /**
     * parts parse result null test case.
     */
    @Test
    public void redisStandaloneConfigurationErrorTest() {
        assertThrows(Throwable.class, () -> {
            ReflectionTestUtils.invokeMethod(redisConnectionFactory, "redisStandaloneConfiguration", new RedisConfigProperties());
        });
    }

    /**
     * redisStandaloneConfiguration property test case.
     */
    @Test
    public void redisStandaloneConfigurationPropertiesTest() {
        RedisStandaloneConfiguration configuration = ReflectionTestUtils.invokeMethod(redisConnectionFactory, "redisStandaloneConfiguration",
                generateRedisConfig(generateDefaultUrl()));
        assertNotNull(configuration);
        assertEquals(DATABASE_TEST_VALUE, configuration.getDatabase());
        assertEquals(LOCALHOST, configuration.getHostName());
        assertEquals(PORT_TEST_VALUE_1, configuration.getPort());
        assertEquals(RedisPassword.of(PASSWORD_TEST_VALUE), configuration.getPassword());
    }

    /**
     * redisStandaloneConfiguration property test case.
     */
    @Test
    public void redisRedisClusterConfigurationPropertiesTest() {
        RedisClusterConfiguration configuration = ReflectionTestUtils.invokeMethod(redisConnectionFactory, "redisClusterConfiguration",
                generateRedisConfig("localhost:2181;localhost:2182"));
        assertNotNull(configuration);
        assertEquals(RedisPassword.of(PASSWORD_TEST_VALUE), configuration.getPassword());
        assertEquals(Collections.unmodifiableSet(Sets.newHashSet(generateRedisNode(PORT_TEST_VALUE_1),
                generateRedisNode(PORT_TEST_VALUE_2))), configuration.getClusterNodes());
    }

    /**
     * genericObjectPoolConfig property test case.
     */
    @Test
    public void getPoolConfigPropertyTest() {
        RedisConfigProperties redisConfigProperties = generateRedisConfig(generateDefaultUrl());
        redisConfigProperties.setMaxWait(1);
        GenericObjectPoolConfig<RedisConnectionFactory> poolConfig = ReflectionTestUtils.invokeMethod(redisConnectionFactory,
                "getPoolConfig", redisConfigProperties);
        assertNotNull(poolConfig);
        assertEquals(DEFAULT_MAX_IDLE, poolConfig.getMaxIdle());
        assertEquals(DEFAULT_MAX_ACTIVE, poolConfig.getMaxTotal());
        assertEquals(DEFAULT_MIN_IDLE, poolConfig.getMinIdle());
        assertEquals(1, poolConfig.getMaxWaitMillis());
    }

    /**
     * redisSentinelConfiguration property test case.
     */
    @Test
    public void redisSentinelConfigurationPropertyTest() {
        RedisSentinelConfiguration configuration = ReflectionTestUtils.invokeMethod(redisConnectionFactory, "redisSentinelConfiguration",
                generateRedisConfig("localhost:2181;localhost:2182"));
        assertNotNull(configuration);
        assertEquals(DATABASE_TEST_VALUE, configuration.getDatabase());
        assertEquals(RedisPassword.of(PASSWORD_TEST_VALUE), configuration.getPassword());
        assertEquals(Collections.unmodifiableSet(Sets.newHashSet(generateRedisNode(PORT_TEST_VALUE_1),
                generateRedisNode(PORT_TEST_VALUE_2))), configuration.getSentinels());
    }

    /**
     * pluginNamed test.
     */
    @Test
    public void pluginNamedTest() {
        assertEquals(PluginEnum.RATE_LIMITER.getName(), new RateLimiterPluginDataHandler().pluginNamed());
    }

    /**
     * url generate by host and port.
     */
    private String generateDefaultUrl() {
        return LOCALHOST + ":" + PORT_TEST_VALUE_1;
    }

    /**
     * generate redisNode.
     */
    private RedisNode generateRedisNode(final int port) {
        return new RedisNode(LOCALHOST, port);
    }

    /**
     * generate RedisConfigProperties.
     */
    private RedisConfigProperties generateRedisConfig(final String url) {
        RedisConfigProperties redisConfigProperties = new RedisConfigProperties();
        redisConfigProperties.setDatabase(DATABASE_TEST_VALUE);
        redisConfigProperties.setUrl(url);
        redisConfigProperties.setMaster(MASTER_TEST_VALUE);
        redisConfigProperties.setPassword(PASSWORD_TEST_VALUE);
        return redisConfigProperties;
    }
}
