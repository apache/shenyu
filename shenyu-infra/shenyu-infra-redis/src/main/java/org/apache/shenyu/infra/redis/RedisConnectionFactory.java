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

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.apache.shenyu.common.enums.RedisModeEnum;
import org.springframework.data.redis.connection.RedisClusterConfiguration;
import org.springframework.data.redis.connection.RedisNode;
import org.springframework.data.redis.connection.RedisPassword;
import org.springframework.data.redis.connection.RedisSentinelConfiguration;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceClientConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettucePoolingClientConfiguration;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * RedisConnectionFactory.
 */
public class RedisConnectionFactory {

    private final LettuceConnectionFactory lettuceConnectionFactory;

    public RedisConnectionFactory(final RedisConfigProperties redisConfigProperties) {
        lettuceConnectionFactory = createLettuceConnectionFactory(redisConfigProperties);
        lettuceConnectionFactory.afterPropertiesSet();
    }

    /**
     * Get Lettuce connection factory.
     * 
     * @return the factory
     */
    public LettuceConnectionFactory getLettuceConnectionFactory() {
        return this.lettuceConnectionFactory;
    }

    private LettuceConnectionFactory createLettuceConnectionFactory(final RedisConfigProperties redisConfigProperties) {
        LettuceClientConfiguration lettuceClientConfiguration = getLettuceClientConfiguration(redisConfigProperties);
        if (RedisModeEnum.SENTINEL.getName().equals(redisConfigProperties.getMode())) {
            return new LettuceConnectionFactory(redisSentinelConfiguration(redisConfigProperties),
                    lettuceClientConfiguration);
        }
        if (RedisModeEnum.CLUSTER.getName().equals(redisConfigProperties.getMode())) {
            return new LettuceConnectionFactory(redisClusterConfiguration(redisConfigProperties),
                    lettuceClientConfiguration);
        }
        return new LettuceConnectionFactory(redisStandaloneConfiguration(redisConfigProperties),
                lettuceClientConfiguration);
    }

    private LettuceClientConfiguration getLettuceClientConfiguration(
            final RedisConfigProperties redisConfigProperties) {
        return LettucePoolingClientConfiguration.builder().poolConfig(getPoolConfig(redisConfigProperties)).build();
    }

    private GenericObjectPoolConfig<?> getPoolConfig(final RedisConfigProperties redisConfigProperties) {
        GenericObjectPoolConfig<?> config = new GenericObjectPoolConfig<>();
        config.setMaxTotal(redisConfigProperties.getMaxActive());
        config.setMaxIdle(redisConfigProperties.getMaxIdle());
        config.setMinIdle(redisConfigProperties.getMinIdle());
        if (Objects.nonNull(redisConfigProperties.getMaxWait())) {
            config.setMaxWait(Duration.ofMillis(redisConfigProperties.getMaxWait().toMillis()));
        }
        return config;
    }

    /**
     * Redis standalone configuration redis standalone configuration.
     *
     * @param redisConfigProperties the rate limiter config
     * @return the redis standalone configuration
     */
    /**
     * Redis standalone configuration redis standalone configuration.
     *
     * @param redisConfigProperties the rate limiter config
     * @return the redis standalone configuration
     */
    protected final RedisStandaloneConfiguration redisStandaloneConfiguration(
            final RedisConfigProperties redisConfigProperties) {
        RedisStandaloneConfiguration config = new RedisStandaloneConfiguration();
        RedisNode redisNode = parseRedisNode(redisConfigProperties.getUrl());
        config.setHostName(redisNode.getHost());
        config.setPort(redisNode.getPort());
        if (Objects.nonNull(redisConfigProperties.getPassword())) {
            config.setPassword(RedisPassword.of(redisConfigProperties.getPassword()));
        }
        config.setDatabase(redisConfigProperties.getDatabase());
        return config;
    }

    private RedisClusterConfiguration redisClusterConfiguration(final RedisConfigProperties redisConfigProperties) {
        RedisClusterConfiguration config = new RedisClusterConfiguration();
        config.setClusterNodes(createRedisNode(redisConfigProperties.getUrl()));
        if (Objects.nonNull(redisConfigProperties.getPassword())) {
            config.setPassword(RedisPassword.of(redisConfigProperties.getPassword()));
        }
        return config;
    }

    private RedisSentinelConfiguration redisSentinelConfiguration(final RedisConfigProperties redisConfigProperties) {
        RedisSentinelConfiguration config = new RedisSentinelConfiguration();
        config.master(redisConfigProperties.getMaster());
        config.setSentinels(createRedisNode(redisConfigProperties.getUrl()));
        if (Objects.nonNull(redisConfigProperties.getPassword())) {
            config.setPassword(RedisPassword.of(redisConfigProperties.getPassword()));
        }
        config.setDatabase(redisConfigProperties.getDatabase());
        return config;
    }

    private List<RedisNode> createRedisNode(final String url) {
        List<RedisNode> redisNodes = new ArrayList<>();
        List<String> nodes = Lists.newArrayList(Splitter.on(";").split(url));
        for (String node : nodes) {
            redisNodes.add(parseRedisNode(node));
        }
        return redisNodes;
    }

    private RedisNode parseRedisNode(final String url) {
        String host = url;
        int port = 6379;
        int lastColonIndex = url.lastIndexOf(":");
        int bracketIndex = url.lastIndexOf("]");
        if (bracketIndex > -1) {
            if (lastColonIndex > bracketIndex) {
                host = url.substring(0, lastColonIndex);
                port = Integer.parseInt(url.substring(lastColonIndex + 1));
            }
        } else {
            if (lastColonIndex > -1) {
                host = url.substring(0, lastColonIndex);
                port = Integer.parseInt(url.substring(lastColonIndex + 1));
            }
        }
        return new RedisNode(host, port);
    }
}
