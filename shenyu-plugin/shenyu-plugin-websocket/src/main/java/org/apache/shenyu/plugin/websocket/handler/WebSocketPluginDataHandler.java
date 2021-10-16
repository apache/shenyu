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

package org.apache.shenyu.plugin.websocket.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.WebSocketUpstream;
import org.apache.shenyu.common.dto.convert.rule.impl.WebSocketRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;

import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * The type WebSocket plugin data handler.
 */
public class WebSocketPluginDataHandler implements PluginDataHandler {
    
    public static final Supplier<CommonHandleCache<String, WebSocketRuleHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);
    
    @Override
    public void handlerSelector(final SelectorData selectorData) {
        List<WebSocketUpstream> upstreamList = GsonUtils.getInstance().fromList(selectorData.getHandle(), WebSocketUpstream.class);
        if (CollectionUtils.isEmpty(upstreamList)) {
            return;
        }
        UpstreamCacheManager.getInstance().submit(selectorData.getId(), convertUpstreamList(upstreamList));
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        UpstreamCacheManager.getInstance().removeByKey(selectorData.getId());
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            WebSocketRuleHandle websocketRuleHandle = GsonUtils.getInstance().fromJson(s, WebSocketRuleHandle.class);
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), websocketRuleHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.WEB_SOCKET.getName();
    }
    
    private List<Upstream> convertUpstreamList(final List<WebSocketUpstream> upstreamList) {
        return upstreamList.stream().map(u -> Upstream.builder()
                .protocol(u.getProtocol())
                .url(u.getUrl())
                .weight(u.getWeight())
                .status(u.isStatus())
                .timestamp(u.getTimestamp())
                .warmup(u.getWarmup())
                .build()).collect(Collectors.toList());
    }
}
