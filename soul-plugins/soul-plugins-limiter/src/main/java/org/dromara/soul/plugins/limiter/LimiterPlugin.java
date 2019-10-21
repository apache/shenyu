/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.soul.plugins.limiter;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.dromara.plugins.api.AbstractSoulPlugin;
import org.dromara.plugins.api.SoulPluginChain;
import org.dromara.soul.cache.api.data.SelectorData;
import org.dromara.soul.cache.api.service.CacheService;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.SoulRequest;
import org.dromara.soul.common.dto.SoulResponse;
import org.dromara.soul.common.dto.convert.RateLimiterHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.result.SoulResult;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.JsonUtils;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.plugins.limiter.config.RedisConfig;
import org.dromara.soul.plugins.limiter.jedis.JedisClient;
import org.dromara.soul.plugins.limiter.jedis.JedisClientCluster;
import org.dromara.soul.plugins.limiter.jedis.JedisClientSentinel;
import org.dromara.soul.plugins.limiter.jedis.JedisClientSingle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import redis.clients.jedis.HostAndPort;
import redis.clients.jedis.JedisCluster;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;
import redis.clients.jedis.JedisSentinelPool;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * @author xiaoyu(Myth)
 */
public class LimiterPlugin extends AbstractSoulPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(LimiterPlugin.class);

    private RedisRateLimiter redisRateLimiter;

    public LimiterPlugin(CacheService cacheService) {
        super(cacheService);
        RedisConfig redisConfig = ConfigEnv.getInstance().getConfig(RedisConfig.class);
        initJedisClient(redisConfig);
        redisRateLimiter = new RedisRateLimiter(initJedisClient(redisConfig));
    }

    @Override
    protected SoulResponse doExecute(SoulRequest soulRequest, SelectorData selectorData, SoulPluginChain chain) {
        String handle = selectorData.getHandle();
        final RateLimiterHandle limiterHandle = GsonUtils.getInstance().fromJson(handle, RateLimiterHandle.class);
        RateLimiterResponse response = redisRateLimiter.isAllowed(selectorData.getId(), limiterHandle.getReplenishRate(), limiterHandle.getBurstCapacity());
        if (!response.isAllowed()) {
            //已经被限流 处理
        }
        return chain.execute(soulRequest);
    }

    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.FUNCTION;
    }

    @Override
    public int getOrder() {
        return PluginEnum.RATE_LIMITER.getOrder();
    }

    @Override
    public String named() {
        return PluginEnum.RATE_LIMITER.getName();
    }


    private JedisClient initJedisClient(final RedisConfig redisConfig) {
        JedisPoolConfig config = new JedisPoolConfig();
        config.setMaxIdle(redisConfig.getMaxIdle());
        config.setMinIdle(redisConfig.getMinIdle());
        config.setMaxTotal(redisConfig.getMaxTotal());
        config.setMaxWaitMillis(redisConfig.getMaxWaitMillis());
        config.setTestOnBorrow(redisConfig.getTestOnBorrow());
        config.setTestOnReturn(redisConfig.getTestOnReturn());
        config.setTestWhileIdle(redisConfig.getTestWhileIdle());
        config.setMinEvictableIdleTimeMillis(redisConfig.getMinEvictableIdleTimeMillis());
        config.setSoftMinEvictableIdleTimeMillis(redisConfig.getSoftMinEvictableIdleTimeMillis());
        config.setTimeBetweenEvictionRunsMillis(redisConfig.getTimeBetweenEvictionRunsMillis());
        config.setNumTestsPerEvictionRun(redisConfig.getNumTestsPerEvictionRun());
        JedisPool jedisPool;
        if (redisConfig.getCluster()) {
            LOGGER.info("build redis cluster ............");
            final String clusterUrl = redisConfig.getClusterUrl();
            final Set<HostAndPort> hostAndPorts =
                    Lists.newArrayList(Splitter.on(";").trimResults().split(clusterUrl))
                            .stream()
                            .map(HostAndPort::parseString).collect(Collectors.toSet());
            JedisCluster jedisCluster = new JedisCluster(hostAndPorts, config);
            return new JedisClientCluster(jedisCluster);
        } else if (redisConfig.getSentinel()) {
            final String sentinelUrl = redisConfig.getSentinelUrl();
            final Set<String> hostAndPorts =
                    new HashSet<>(Lists.newArrayList(Splitter.on(";").split(sentinelUrl)));
            JedisSentinelPool pool =
                    new JedisSentinelPool(redisConfig.getMasterName(), hostAndPorts,
                            config, redisConfig.getTimeOut(), redisConfig.getPassword());
            return new JedisClientSentinel(pool);
        } else {
            if (StringUtils.isNoneBlank(redisConfig.getPassword())) {
                jedisPool = new JedisPool(config, redisConfig.getHostName(), redisConfig.getPort(), redisConfig.getTimeOut(), redisConfig.getPassword());
            } else {
                jedisPool = new JedisPool(config, redisConfig.getHostName(), redisConfig.getPort(), redisConfig.getTimeOut());
            }
            return new JedisClientSingle(jedisPool);
        }
    }


}
