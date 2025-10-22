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

package org.apache.shenyu.plugin.ai.proxy.enhanced.handler;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.ChatClientCache;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;

import java.util.Objects;

/**
 * this is ai proxy plugin handler.
 */
public class AiProxyPluginHandler implements PluginDataHandler {

    private final CommonHandleCache<String, AiProxyHandle> selectorCachedHandle = new CommonHandleCache<>();

    private final ChatClientCache chatClientCache;

    public AiProxyPluginHandler(final ChatClientCache chatClientCache) {
        this.chatClientCache = chatClientCache;
    }

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        // Note: The logic for handling global plugin configuration with Singleton has been removed
        // as it's part of the legacy pattern we are moving away from.
        // No global plugin configuration handling is performed.
    }

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        // Invalidate the cache first when the selector is updated.
        chatClientCache.remove(selectorData.getId());
        // Do NOT remove AiProxyApiKeyCache here. Admin will push updated AI_PROXY_API_KEY events
        // with refreshed realApiKey after selector changes. Removing here introduces a window of misses.
        if (Objects.isNull(selectorData.getHandle())) {
            return;
        }
        AiProxyHandle aiProxyHandle = GsonUtils.getInstance().fromJson(selectorData.getHandle(), AiProxyHandle.class);
        aiProxyHandle.normalize();
        selectorCachedHandle
                .cachedHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE), aiProxyHandle);
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        // Invalidate the cache when the selector is removed.
        chatClientCache.remove(selectorData.getId());
        selectorCachedHandle
                .removeHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.AI_PROXY.getName();
    }

    /**
     * Gets selector cached handle.
     *
     * @return the selector cached handle
     */
    public CommonHandleCache<String, AiProxyHandle> getSelectorCachedHandle() {
        return selectorCachedHandle;
    }
}
