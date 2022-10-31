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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.RateLimiterHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.redis.RedisConfigProperties;
import org.apache.shenyu.plugin.cache.redis.RedisConnectionFactory;
import org.apache.shenyu.plugin.cache.redis.serializer.ShenyuRedisSerializationContext;
import org.springframework.data.redis.core.ReactiveRedisTemplate;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * The type Rate limiter plugin data handler.
 */
public class RateLimiterPluginDataHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, RateLimiterHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.nonNull(pluginData) && Boolean.TRUE.equals(pluginData.getEnabled())) {
            //init redis
            RedisConfigProperties redisConfigProperties = GsonUtils.getInstance().fromJson(pluginData.getConfig(), RedisConfigProperties.class);
            //spring data redisTemplate
            if (Objects.isNull(Singleton.INST.get(ReactiveRedisTemplate.class))
                    || Objects.isNull(Singleton.INST.get(RedisConfigProperties.class))
                    || !redisConfigProperties.equals(Singleton.INST.get(RedisConfigProperties.class))) {
                final RedisConnectionFactory redisConnectionFactory = new RedisConnectionFactory(redisConfigProperties);
                ReactiveRedisTemplate<String, String> reactiveRedisTemplate = new ShenyuReactiveRedisTemplate<>(
                        redisConnectionFactory.getLettuceConnectionFactory(),
                        ShenyuRedisSerializationContext.stringSerializationContext());
                Singleton.INST.single(ReactiveRedisTemplate.class, reactiveRedisTemplate);
                Singleton.INST.single(RedisConfigProperties.class, redisConfigProperties);
            }
        }
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            final RateLimiterHandle rateLimiterHandle = GsonUtils.getInstance().fromJson(s, RateLimiterHandle.class);
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), rateLimiterHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.RATE_LIMITER.getName();
    }
}
