/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.config;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.dromara.soul.common.config.DubboRegisterConfig;
import org.dromara.soul.common.config.MonitorConfig;
import org.dromara.soul.common.config.RateLimiterConfig;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RedisModeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.web.plugin.dubbo.ApplicationConfigCache;
import org.influxdb.dto.Point;
import org.springframework.data.influxdb.InfluxDBConnectionFactory;
import org.springframework.data.influxdb.InfluxDBProperties;
import org.springframework.data.influxdb.InfluxDBTemplate;
import org.springframework.data.influxdb.converter.PointConverter;
import org.springframework.data.redis.connection.RedisClusterConfiguration;
import org.springframework.data.redis.connection.RedisNode;
import org.springframework.data.redis.connection.RedisPassword;
import org.springframework.data.redis.connection.RedisSentinelConfiguration;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceClientConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettucePoolingClientConfiguration;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * The enum Plugin config init.
 *
 * @author xiaoyu(Myth)
 */
public enum PluginConfigHandler {

    /**
     * Ins plugin config init.
     */
    INS;

    /**
     * Init plugin config.
     *
     * @param pluginDataList the plugin data list
     */
    public void initPluginConfig(final List<PluginData> pluginDataList) {
        pluginDataList.stream()
                .filter(PluginData::getEnabled)
                .forEach(pluginData -> {
                    if (PluginEnum.MONITOR.getName().equals(pluginData.getName())) {
                        MonitorConfig monitorConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), MonitorConfig.class);
                        if (Objects.isNull(Singleton.INST.get(InfluxDBTemplate.class))
                                || Objects.isNull(Singleton.INST.get(MonitorConfig.class))
                                || !monitorConfig.equals(Singleton.INST.get(MonitorConfig.class))) {
                            InfluxDBConnectionFactory connectionFactory = new InfluxDBConnectionFactory(buildByConfig(monitorConfig));
                            InfluxDBTemplate<Point> influxDBTemplate = new InfluxDBTemplate<>(connectionFactory, new PointConverter());
                            Singleton.INST.single(InfluxDBTemplate.class, influxDBTemplate);
                            Singleton.INST.single(MonitorConfig.class, monitorConfig);
                        }
                    } else if (PluginEnum.RATE_LIMITER.getName().equals(pluginData.getName())) {
                        //初始化redis
                        RateLimiterConfig rateLimiterConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), RateLimiterConfig.class);
                        //出来转换成spring data redisTemplate
                        if (Objects.isNull(Singleton.INST.get(ReactiveRedisTemplate.class))
                                || Objects.isNull(Singleton.INST.get(RateLimiterConfig.class))
                                || !rateLimiterConfig.equals(Singleton.INST.get(RateLimiterConfig.class))) {
                            LettuceConnectionFactory lettuceConnectionFactory = createLettuceConnectionFactory(rateLimiterConfig);
                            lettuceConnectionFactory.afterPropertiesSet();
                            RedisSerializer<String> serializer = new StringRedisSerializer();
                            RedisSerializationContext<String, String> serializationContext = RedisSerializationContext
                                    .<String, String>newSerializationContext()
                                    .key(serializer)
                                    .value(serializer)
                                    .hashKey(serializer)
                                    .hashValue(serializer)
                                    .build();
                            ReactiveRedisTemplate<String, String> reactiveRedisTemplate = new ReactiveRedisTemplate<>(lettuceConnectionFactory, serializationContext);
                            Singleton.INST.single(ReactiveRedisTemplate.class, reactiveRedisTemplate);
                            Singleton.INST.single(RateLimiterConfig.class, rateLimiterConfig);
                        }
                    } else if (PluginEnum.DUBBO.getName().equals(pluginData.getName())) {
                        DubboRegisterConfig dubboRegisterConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), DubboRegisterConfig.class);
                        DubboRegisterConfig exist = Singleton.INST.get(DubboRegisterConfig.class);
                        if (Objects.nonNull(dubboRegisterConfig)) {
                            if (Objects.isNull(exist)
                                    || !dubboRegisterConfig.equals(exist)) {
                                //如果是空，进行初始化操作，
                                ApplicationConfigCache.getInstance().init(dubboRegisterConfig.getRegister());
                                ApplicationConfigCache.getInstance().invalidateAll();
                            }
                            Singleton.INST.single(DubboRegisterConfig.class, dubboRegisterConfig);
                        }
                    }
                });

    }

    private LettuceConnectionFactory createLettuceConnectionFactory(final RateLimiterConfig rateLimiterConfig) {
        LettuceClientConfiguration lettuceClientConfiguration = getLettuceClientConfiguration(rateLimiterConfig);
        if (RedisModeEnum.SENTINEL.getName().equals(rateLimiterConfig.getMode())) {
            return new LettuceConnectionFactory(redisSentinelConfiguration(rateLimiterConfig), lettuceClientConfiguration);
        } else if (RedisModeEnum.CLUSTER.getName().equals(rateLimiterConfig.getMode())) {
            return new LettuceConnectionFactory(redisClusterConfiguration(rateLimiterConfig), lettuceClientConfiguration);
        }
        return new LettuceConnectionFactory(redisStandaloneConfiguration(rateLimiterConfig), lettuceClientConfiguration);
    }

    private LettuceClientConfiguration getLettuceClientConfiguration(final RateLimiterConfig rateLimiterConfig) {
        return LettucePoolingClientConfiguration.builder()
                .poolConfig(getPoolConfig(rateLimiterConfig))
                .build();
    }

    private GenericObjectPoolConfig<?> getPoolConfig(final RateLimiterConfig rateLimiterConfig) {
        GenericObjectPoolConfig<?> config = new GenericObjectPoolConfig<>();
        config.setMaxTotal(rateLimiterConfig.getMaxActive());
        config.setMaxIdle(rateLimiterConfig.getMaxIdle());
        config.setMinIdle(rateLimiterConfig.getMinIdle());
        if (rateLimiterConfig.getMaxWait() != null) {
            config.setMaxWaitMillis(rateLimiterConfig.getMaxWait().toMillis());
        }
        return config;
    }

    protected final RedisStandaloneConfiguration redisStandaloneConfiguration(final RateLimiterConfig rateLimiterConfig) {
        RedisStandaloneConfiguration config = new RedisStandaloneConfiguration();
        String[] parts = StringUtils.split(rateLimiterConfig.getUrl(), ":");
        assert parts != null;
        config.setHostName(parts[0]);
        config.setPort(Integer.parseInt(parts[1]));
        if (rateLimiterConfig.getPassword() != null) {
            config.setPassword(RedisPassword.of(rateLimiterConfig.getPassword()));
        }
        config.setDatabase(rateLimiterConfig.getDatabase());
        return config;
    }

    private RedisClusterConfiguration redisClusterConfiguration(final RateLimiterConfig rateLimiterConfig) {
        RedisClusterConfiguration config = new RedisClusterConfiguration();
        config.setClusterNodes(createRedisNode(rateLimiterConfig.getUrl()));
        if (rateLimiterConfig.getPassword() != null) {
            config.setPassword(RedisPassword.of(rateLimiterConfig.getPassword()));
        }
        return config;
    }

    private RedisSentinelConfiguration redisSentinelConfiguration(final RateLimiterConfig rateLimiterConfig) {
        RedisSentinelConfiguration config = new RedisSentinelConfiguration();
        config.master(rateLimiterConfig.getMaster());
        config.setSentinels(createRedisNode(rateLimiterConfig.getUrl()));
        if (rateLimiterConfig.getPassword() != null) {
            config.setPassword(RedisPassword.of(rateLimiterConfig.getPassword()));
        }
        config.setDatabase(rateLimiterConfig.getDatabase());
        return config;
    }

    private List<RedisNode> createRedisNode(final String url) {
        List<RedisNode> redisNodes = new ArrayList<>();
        List<String> nodes = Lists.newArrayList(Splitter.on(";").split(url));
        for (String node : nodes) {
            try {
                String[] parts = StringUtils.split(node, ":");
                Assert.state(Objects.requireNonNull(parts).length == 2, "Must be defined as 'host:port'");
                redisNodes.add(new RedisNode(parts[0], Integer.parseInt(parts[1])));
            } catch (RuntimeException ex) {
                throw new IllegalStateException(
                        "Invalid redis sentinel " + "property '" + node + "'", ex);
            }
        }
        return redisNodes;
    }

    private InfluxDBProperties buildByConfig(final MonitorConfig monitorConfig) {
        InfluxDBProperties influxDBProperties = new InfluxDBProperties();
        influxDBProperties.setDatabase(monitorConfig.getDatabase());
        influxDBProperties.setUrl(monitorConfig.getUrl());
        influxDBProperties.setUsername(monitorConfig.getUserName());
        influxDBProperties.setPassword(monitorConfig.getPassword());
        influxDBProperties.setConnectTimeout(monitorConfig.getConnectTimeout());
        influxDBProperties.setReadTimeout(monitorConfig.getReadTimeout());
        influxDBProperties.setRetentionPolicy(monitorConfig.getRetentionPolicy());
        influxDBProperties.setWriteTimeout(monitorConfig.getWriteTimeout());
        influxDBProperties.setGzip(monitorConfig.getGzip());
        return influxDBProperties;
    }
}
