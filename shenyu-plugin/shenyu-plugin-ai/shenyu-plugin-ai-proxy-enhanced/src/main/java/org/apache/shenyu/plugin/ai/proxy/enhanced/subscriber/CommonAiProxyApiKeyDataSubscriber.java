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

package org.apache.shenyu.plugin.ai.proxy.enhanced.subscriber;

import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.ChatClientCache;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;

import java.util.List;
import java.util.Objects;

/**
 * Common AI proxy api key data subscriber.
 */
public class CommonAiProxyApiKeyDataSubscriber implements AiProxyApiKeyDataSubscriber {

    private final ChatClientCache chatClientCache;

    private final AiProxyApiKeyCache aiProxyApiKeyCache;

    public CommonAiProxyApiKeyDataSubscriber(final ChatClientCache chatClientCache, final AiProxyApiKeyCache aiProxyApiKeyCache) {
        this.chatClientCache = chatClientCache;
        this.aiProxyApiKeyCache = aiProxyApiKeyCache;
    }


    @Override
    public void onSubscribe(final ProxyApiKeyData proxyApiKeyData) {
        if (Objects.isNull(proxyApiKeyData)) {
            return;
        }
        aiProxyApiKeyCache.onSubscribe(proxyApiKeyData);
        chatClientCache.remove(proxyApiKeyData.getSelectorId());
    }

    @Override
    public void unSubscribe(final ProxyApiKeyData proxyApiKeyData) {
        if (Objects.isNull(proxyApiKeyData)) {
            return;
        }
        aiProxyApiKeyCache.unSubscribe(proxyApiKeyData);
        chatClientCache.remove(proxyApiKeyData.getSelectorId());
    }

    @Override
    public void refresh(final List<ProxyApiKeyData> proxyApiKeyDataList) {
        for (final ProxyApiKeyData proxyApiKeyData : proxyApiKeyDataList) {
            aiProxyApiKeyCache.onSubscribe(proxyApiKeyData);
            chatClientCache.remove(proxyApiKeyData.getSelectorId());
        }
    }
}
