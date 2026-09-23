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

package org.apache.shenyu.plugin.ai.sensitive.word.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.SensitiveWordHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.infra.redis.RedisConfigProperties;
import org.apache.shenyu.infra.redis.RedisConnectionFactory;
import org.apache.shenyu.infra.redis.ShenyuReactiveRedisTemplate;
import org.apache.shenyu.infra.redis.serializer.ShenyuRedisSerializationContext;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.ai.sensitive.word.ac.AhoCorasick;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * The sensitive word plugin data handler.
 */
public class SensitiveWordPluginDataHandler implements PluginDataHandler {

    /**
     * The plugin name, it is also the cache key of the redis template.
     */
    public static final String PLUGIN_NAME = PluginEnum.SENSITIVE_WORD.getName();

    /**
     * Cache the reactive redis template, the key is the plugin name.
     */
    public static final Supplier<CommonHandleCache<String, ReactiveRedisTemplate<String, String>>> REDIS_TEMPLATES =
            new BeanHolder<>(CommonHandleCache::new);

    /**
     * Cache the redis configuration the template was built from, the key is the plugin name.
     */
    public static final Supplier<CommonHandleCache<String, RedisConfigProperties>> REDIS_PROPERTIES =
            new BeanHolder<>(CommonHandleCache::new);

    /**
     * Cache the rule handle, the key is the cache key of the rule.
     */
    public static final Supplier<CommonHandleCache<String, SensitiveWordHandle>> CACHED_HANDLE =
            new BeanHolder<>(CommonHandleCache::new);

    /**
     * Cache the dictionary automaton, the key is the redis key of the dictionary, so that rules
     * pointing to different dictionaries never share the same automaton.
     */
    public static final Supplier<CommonHandleCache<String, CachedDictionary>> DICTIONARIES =
            new BeanHolder<>(CommonHandleCache::new);

    private static final Logger LOG = LoggerFactory.getLogger(SensitiveWordPluginDataHandler.class);

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.isNull(pluginData) || !Boolean.TRUE.equals(pluginData.getEnabled())) {
            return;
        }
        RedisConfigProperties redisConfig = GsonUtils.getInstance()
                .fromJson(pluginData.getConfig(), RedisConfigProperties.class);
        if (Objects.isNull(redisConfig) || !StringUtils.hasText(redisConfig.getUrl())) {
            LOG.warn("sensitive word plugin: the redis configuration is missing, skip the redis initialization");
            return;
        }
        RedisConfigProperties cachedProperties = REDIS_PROPERTIES.get().obtainHandle(PLUGIN_NAME);
        ReactiveRedisTemplate<String, String> cachedTemplate = REDIS_TEMPLATES.get().obtainHandle(PLUGIN_NAME);
        if (Objects.isNull(cachedTemplate) || !redisConfig.equals(cachedProperties)) {
            RedisConnectionFactory connectionFactory = new RedisConnectionFactory(redisConfig);
            ReactiveRedisTemplate<String, String> redisTemplate = new ShenyuReactiveRedisTemplate<>(
                    connectionFactory.getLettuceConnectionFactory(),
                    ShenyuRedisSerializationContext.stringSerializationContext());
            REDIS_TEMPLATES.get().cachedHandle(PLUGIN_NAME, redisTemplate);
            REDIS_PROPERTIES.get().cachedHandle(PLUGIN_NAME, redisConfig);
            // the client that is replaced must not keep its connection pool and its threads alive
            if (Objects.nonNull(cachedTemplate)) {
                RedisConnectionFactory.destroyQuietly(cachedTemplate.getConnectionFactory());
            }
            LOG.info("sensitive word plugin: cached the reactive redis template");
        }
    }

    @Override
    public void removePlugin(final PluginData pluginData) {
        ReactiveRedisTemplate<String, String> cachedTemplate = REDIS_TEMPLATES.get().obtainHandle(PLUGIN_NAME);
        if (Objects.nonNull(cachedTemplate)) {
            RedisConnectionFactory.destroyQuietly(cachedTemplate.getConnectionFactory());
        }
        REDIS_TEMPLATES.get().removeHandle(PLUGIN_NAME);
        REDIS_PROPERTIES.get().removeHandle(PLUGIN_NAME);
        LOG.info("sensitive word plugin: released the cached redis template");
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(json -> {
            SensitiveWordHandle handle = GsonUtils.getInstance().fromJson(json, SensitiveWordHandle.class);
            if (Objects.isNull(handle)) {
                handle = SensitiveWordHandle.newDefaultInstance();
            }
            SensitiveWordHandle previous = CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);
            // The rule changed, drop the cached dictionary of the previous and of the new
            // configuration, so that both are read from redis again.
            if (Objects.nonNull(previous)) {
                DICTIONARIES.get().removeHandle(previous.dictionaryKey());
            }
            DICTIONARIES.get().removeHandle(handle.dictionaryKey());
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
    }

    @Override
    public String pluginNamed() {
        return PLUGIN_NAME;
    }

    /**
     * A dictionary automaton together with the time it was loaded, used to refresh a dictionary
     * whose refresh interval elapsed.
     */
    public static final class CachedDictionary {

        private final AhoCorasick automaton;

        private final long loadTime;

        public CachedDictionary(final AhoCorasick automaton) {
            this.automaton = automaton;
            this.loadTime = System.currentTimeMillis();
        }

        /**
         * Whether this dictionary is older than the refresh interval of the rule.
         *
         * @param refreshIntervalSeconds the refresh interval in seconds
         * @return true when the dictionary must be read from redis again
         */
        public boolean isExpired(final long refreshIntervalSeconds) {
            return refreshIntervalSeconds <= 0L
                    || System.currentTimeMillis() - loadTime >= refreshIntervalSeconds * 1000L;
        }

        /**
         * get the automaton.
         *
         * @return the automaton
         */
        public AhoCorasick getAutomaton() {
            return automaton;
        }
    }
}
