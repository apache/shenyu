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
import org.apache.shenyu.plugin.ratelimiter.config.RateLimiterConfig;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.redis.connection.RedisClusterConfiguration;
import org.springframework.data.redis.connection.RedisNode;
import org.springframework.data.redis.connection.RedisPassword;
import org.springframework.data.redis.connection.RedisSentinelConfiguration;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.Duration;
import java.util.Collections;

/**
 * RateLimiterPluginDataHandler test.
 */
@RunWith(MockitoJUnitRunner.class)
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

    private RateLimiterPluginDataHandler rateLimiterPluginDataHandler;

    @Before
    public void setUp() {
        this.rateLimiterPluginDataHandler = new RateLimiterPluginDataHandler();
    }

    /**
     * handlerPlugin Singleton.INST init test case.
     */
    @Test
    public void handlerPluginTest() {
        RateLimiterConfig rateLimiterConfig = generateRateLimiterConfig(generateDefaultUrl());
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig(GsonUtils.getInstance().toJson(rateLimiterConfig));
        rateLimiterPluginDataHandler.handlerPlugin(pluginData);
        Assert.assertEquals(rateLimiterConfig.getUrl(), Singleton.INST.get(RateLimiterConfig.class).getUrl());
        Assert.assertNotNull(Singleton.INST.get(ReactiveRedisTemplate.class));
    }

    /**
     * parts parse result null test case.
     */
    @Test(expected = Throwable.class)
    public void redisStandaloneConfigurationErrorTest() {
        ReflectionTestUtils.invokeMethod(rateLimiterPluginDataHandler, "redisStandaloneConfiguration", new RateLimiterConfig());
    }

    /**
     * redisStandaloneConfiguration property test case.
     */
    @Test
    public void redisStandaloneConfigurationPropertiesTest() {
        RedisStandaloneConfiguration configuration = ReflectionTestUtils.invokeMethod(rateLimiterPluginDataHandler, "redisStandaloneConfiguration",
                generateRateLimiterConfig(generateDefaultUrl()));
        Assert.assertEquals(DATABASE_TEST_VALUE, configuration.getDatabase());
        Assert.assertEquals(LOCALHOST, configuration.getHostName());
        Assert.assertEquals(PORT_TEST_VALUE_1, configuration.getPort());
        Assert.assertEquals(RedisPassword.of(PASSWORD_TEST_VALUE), configuration.getPassword());
    }

    /**
     * redisStandaloneConfiguration property test case.
     */
    @Test
    public void redisRedisClusterConfigurationPropertiesTest() {
        RedisClusterConfiguration configuration = ReflectionTestUtils.invokeMethod(rateLimiterPluginDataHandler, "redisClusterConfiguration",
                generateRateLimiterConfig("localhost:2181;localhost:2182"));
        Assert.assertEquals(RedisPassword.of(PASSWORD_TEST_VALUE), configuration.getPassword());
        Assert.assertEquals(Collections.unmodifiableSet(Sets.newHashSet(generateRedisNode(PORT_TEST_VALUE_1),
                generateRedisNode(PORT_TEST_VALUE_2))), configuration.getClusterNodes());
    }

    /**
     * genericObjectPoolConfig property test case.
     */
    @Test
    public void getPoolConfigPropertyTest() {
        Duration duration = Duration.ofHours(1);
        RateLimiterConfig rateLimiterConfig = new RateLimiterConfig();
        rateLimiterConfig.setMaxWait(duration);
        GenericObjectPoolConfig poolConfig = ReflectionTestUtils.invokeMethod(rateLimiterPluginDataHandler,
                "getPoolConfig", rateLimiterConfig);
        Assert.assertEquals(DEFAULT_MAX_IDLE, poolConfig.getMaxIdle());
        Assert.assertEquals(DEFAULT_MAX_ACTIVE, poolConfig.getMaxTotal());
        Assert.assertEquals(DEFAULT_MIN_IDLE, poolConfig.getMinIdle());
        Assert.assertEquals(duration.toMillis(), poolConfig.getMaxWaitMillis());
    }

    /**
     * redisSentinelConfiguration property test case.
     */
    @Test
    public void redisSentinelConfigurationPropertyTest() {
        RedisSentinelConfiguration configuration = ReflectionTestUtils.invokeMethod(rateLimiterPluginDataHandler, "redisSentinelConfiguration",
                generateRateLimiterConfig("localhost:2181;localhost:2182"));
        Assert.assertEquals(DATABASE_TEST_VALUE, configuration.getDatabase());
        Assert.assertEquals(RedisPassword.of(PASSWORD_TEST_VALUE), configuration.getPassword());
        Assert.assertEquals(Collections.unmodifiableSet(Sets.newHashSet(generateRedisNode(PORT_TEST_VALUE_1),
                generateRedisNode(PORT_TEST_VALUE_2))), configuration.getSentinels());
    }

    /**
     * pluginNamed test.
     */
    @Test
    public void pluginNamedTest() {
        Assert.assertEquals(PluginEnum.RATE_LIMITER.getName(), rateLimiterPluginDataHandler.pluginNamed());
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
     * generate RateLimiterConfig.
     */
    private RateLimiterConfig generateRateLimiterConfig(final String url) {
        RateLimiterConfig rateLimiterConfig = new RateLimiterConfig();
        rateLimiterConfig.setDatabase(DATABASE_TEST_VALUE);
        rateLimiterConfig.setUrl(url);
        rateLimiterConfig.setMaster(MASTER_TEST_VALUE);
        rateLimiterConfig.setPassword(PASSWORD_TEST_VALUE);
        return rateLimiterConfig;
    }
}
