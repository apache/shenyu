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

package org.apache.shenyu.plugin.ai.statistic.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.statistic.redis.RedisConfigProperties;
import org.apache.shenyu.plugin.ai.statistic.redis.RedisConnectionFactory;
import org.apache.shenyu.plugin.ai.statistic.redis.serializer.ShenyuRedisSerializationContext;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;

import java.util.Objects;
import java.util.function.Supplier;

/**
 * Shenyu ai statistic plugin handler.
 */
public class AiStatisticPluginHandler implements PluginDataHandler {
    
    private static final Logger LOG = LoggerFactory.getLogger(AiStatisticPluginHandler.class);
    
    public static final Supplier<CommonHandleCache<String, ReactiveRedisTemplate>> REDIS_CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);
    
    public static final Supplier<CommonHandleCache<String, RedisConfigProperties>> REDIS_PROPERTIES_CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);
    
    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.nonNull(pluginData) && Boolean.TRUE.equals(pluginData.getEnabled())) {
            //init redis
            RedisConfigProperties redisConfigProperties = GsonUtils.getInstance().fromJson(pluginData.getConfig(), RedisConfigProperties.class);
            //spring data redisTemplate
            if (Objects.isNull(REDIS_CACHED_HANDLE.get().obtainHandle(PluginEnum.AI_STATISTIC.getName()))
                    || Objects.isNull(REDIS_PROPERTIES_CACHED_HANDLE.get().obtainHandle(PluginEnum.AI_STATISTIC.getName()))
                    || !redisConfigProperties.equals(REDIS_PROPERTIES_CACHED_HANDLE.get().obtainHandle(PluginEnum.AI_STATISTIC.getName()))) {
                final RedisConnectionFactory redisConnectionFactory = new RedisConnectionFactory(redisConfigProperties);
                ReactiveRedisTemplate<String, String> reactiveRedisTemplate = new ReactiveRedisTemplate<>(
                        redisConnectionFactory.getLettuceConnectionFactory(),
                        ShenyuRedisSerializationContext.stringSerializationContext());
                REDIS_CACHED_HANDLE.get().cachedHandle(PluginEnum.AI_STATISTIC.getName(), reactiveRedisTemplate);
                REDIS_PROPERTIES_CACHED_HANDLE.get().cachedHandle(PluginEnum.AI_STATISTIC.getName(), redisConfigProperties);
            }
        }
    }
    
    @Override
    public String pluginNamed() {
        return PluginEnum.AI_STATISTIC.getName();
    }
}
