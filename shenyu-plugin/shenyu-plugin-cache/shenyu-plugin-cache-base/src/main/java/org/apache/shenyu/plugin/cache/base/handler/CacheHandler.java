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

package org.apache.shenyu.plugin.cache.base.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.cache.base.ICache;
import org.apache.shenyu.plugin.cache.base.config.CacheConfig;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

/**
 * the cache Handler.
 */
public class CacheHandler implements PluginDataHandler {

    /**
     * the log.
     */
    private static final Logger LOG = LoggerFactory.getLogger(CacheHandler.class);

    /**
     * Handler plugin.
     *
     * @param pluginData the plugin data
     */
    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.isNull(pluginData) || Boolean.FALSE.equals(pluginData.getEnabled())) {
            LOG.info("the plugin {} is disabled", this.pluginNamed());
            return;
        }
        CacheConfig cacheConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), CacheConfig.class);
        if (Objects.isNull(cacheConfig)) {
            LOG.info("invalid cacheConfig.");
            return;
        }
        LOG.info("use the {} cache.", cacheConfig.getCacheType());
        final CacheConfig lastCacheConfig = Singleton.INST.get(CacheConfig.class);
        if (cacheConfig.equals(lastCacheConfig)) {
            LOG.info("cache plugin initialized.");
            return;
        }
        Singleton.INST.single(CacheConfig.class, cacheConfig);
        if (Objects.nonNull(lastCacheConfig)) {
            final ICache cache = ExtensionLoader.getExtensionLoader(ICache.class).getJoin(cacheConfig.getCacheType());
            cache.refresh();
        }
    }

    /**
     * Plugin named string.
     *
     * @return the string
     */
    @Override
    public String pluginNamed() {
        return PluginEnum.CACHE.getName();
    }
}
