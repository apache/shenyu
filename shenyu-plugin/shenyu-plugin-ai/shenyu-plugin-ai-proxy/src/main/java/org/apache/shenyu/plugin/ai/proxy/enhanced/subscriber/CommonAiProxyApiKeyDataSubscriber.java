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

import java.util.Objects;

/** 
 * CommonAiProxyApiKeyDataSubscriber updates local cache for proxy api key mappings.
 */
public final class CommonAiProxyApiKeyDataSubscriber implements AiProxyApiKeyDataSubscriber {

    private final ChatClientCache chatClientCache;

    public CommonAiProxyApiKeyDataSubscriber(final ChatClientCache chatClientCache) {
        this.chatClientCache = chatClientCache;
    }

    @Override
    public void onSubscribe(final ProxyApiKeyData data) {
        if (Objects.isNull(data) || Objects.isNull(data.getProxyApiKey())) {
            return;
        }
        AiProxyApiKeyCache.getInstance().cache(data);
    }

    @Override
    public void unSubscribe(final ProxyApiKeyData data) {
        if (Objects.isNull(data) || Objects.isNull(data.getProxyApiKey())) {
            return;
        }
        AiProxyApiKeyCache.getInstance().remove(data);
    }

    @Override
    public void refresh() {
        AiProxyApiKeyCache.getInstance().refresh();
        chatClientCache.clearAll();
    }
}
