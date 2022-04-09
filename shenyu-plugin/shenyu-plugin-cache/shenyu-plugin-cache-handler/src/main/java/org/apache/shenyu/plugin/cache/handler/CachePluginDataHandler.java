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

package org.apache.shenyu.plugin.cache.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.impl.CacheRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.ICache;
import org.apache.shenyu.plugin.cache.ICacheBuilder;
import org.apache.shenyu.plugin.cache.config.CacheConfig;
import org.apache.shenyu.plugin.cache.utils.CacheUtils;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * The type Cache plugin data handler.
 */
public class CachePluginDataHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, CacheRuleHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    /**
     * the log.
     */
    private static final Logger LOG = LoggerFactory.getLogger(CachePluginDataHandler.class);

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.isNull(pluginData) || Boolean.FALSE.equals(pluginData.getEnabled())) {
            LOG.info("the plugin {} is disabled", this.pluginNamed());
            this.closeCacheIfNeed();
            return;
        }
        final String config = pluginData.getConfig();
        CacheConfig cacheConfig = GsonUtils.getInstance().fromJson(config, CacheConfig.class);
        if (Objects.isNull(cacheConfig)) {
            LOG.info("invalid cacheConfig.");
            return;
        }
        LOG.info("use the {} cache.", cacheConfig.getCacheType());
        // set the config to compare with lastConfig.
        cacheConfig.setConfig(config);
        final CacheConfig lastCacheConfig = Singleton.INST.get(CacheConfig.class);
        if (cacheConfig.equals(lastCacheConfig)) {
            LOG.info("cache plugin initialized.");
            return;
        }
        Singleton.INST.single(CacheConfig.class, cacheConfig);
        this.closeCacheIfNeed();
        final ICacheBuilder cacheBuilder = ExtensionLoader.getExtensionLoader(ICacheBuilder.class).getJoin(cacheConfig.getCacheType());
        Singleton.INST.single(ICache.class, cacheBuilder.builderCache(config));
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(json -> {
            final CacheRuleHandle cacheRuleHandle = GsonUtils.getInstance().fromJson(json, CacheRuleHandle.class);
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), cacheRuleHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(json -> CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.CACHE.getName();
    }

    /**
     * close the cache if you need.
     */
    private void closeCacheIfNeed() {
        ICache lastCache = CacheUtils.getCache();
        if (Objects.nonNull(lastCache)) {
            // close last cache.
            LOG.info("close the last cache {}", lastCache);
            lastCache.close();
        }
    }
}
