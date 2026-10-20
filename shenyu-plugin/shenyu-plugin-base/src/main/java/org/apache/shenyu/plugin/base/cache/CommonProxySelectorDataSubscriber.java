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

package org.apache.shenyu.plugin.base.cache;

import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.plugin.base.handler.ProxySelectorDataHandler;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * CommonProxySelectorDataSubscriber.
 */
public class CommonProxySelectorDataSubscriber implements ProxySelectorDataSubscriber {

    private final Map<String, ProxySelectorDataHandler> handlerMap;

    // Node and path deletion events identify selectors by id; handlers remove resources by name.
    private final Map<String, Map<String, String>> selectorNames = new ConcurrentHashMap<>();

    public CommonProxySelectorDataSubscriber(final List<ProxySelectorDataHandler> proxySelectorDataHandlerList) {
        this.handlerMap = proxySelectorDataHandlerList.stream().collect(Collectors.toConcurrentMap(ProxySelectorDataHandler::pluginName, e -> e));
    }

    @Override
    public void onSubscribe(final ProxySelectorData proxySelectorData) {
        Optional.ofNullable(handlerMap.get(proxySelectorData.getPluginName()))
                .ifPresent(handler -> {
                    handler.handlerProxySelector(proxySelectorData);
                    if (Objects.nonNull(proxySelectorData.getId()) && Objects.nonNull(proxySelectorData.getName())) {
                        selectorNames.computeIfAbsent(proxySelectorData.getPluginName(), key -> new ConcurrentHashMap<>())
                                .put(proxySelectorData.getId(), proxySelectorData.getName());
                    }
                });
    }

    @Override
    public void unSubscribe(final ProxySelectorData proxySelectorData) {
        Optional.ofNullable(handlerMap.get(proxySelectorData.getPluginName()))
                .ifPresent(handler -> {
                    Map<String, String> names = selectorNames.get(proxySelectorData.getPluginName());
                    String id = proxySelectorData.getId();
                    String cachedName = Objects.nonNull(names) && Objects.nonNull(id) ? names.get(id) : null;
                    String name = Objects.nonNull(cachedName) ? cachedName : proxySelectorData.getName();
                    if (Objects.isNull(name)) {
                        return;
                    }
                    handler.removeProxySelector(name);
                    if (Objects.nonNull(names)) {
                        if (Objects.nonNull(id)) {
                            names.remove(id);
                        } else {
                            names.values().removeIf(name::equals);
                        }
                    }
                });
    }

    @Override
    public void refresh() {
        handlerMap.values().forEach(ProxySelectorDataHandler::refresh);
        selectorNames.clear();
    }
}
