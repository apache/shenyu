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

package org.apache.shenyu.web.handler;

import com.google.common.collect.Sets;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.PluginHandlerEventEnums;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.PluginHandlerEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebHandler;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.util.*;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.stream.Collectors;

/**
 * This is web handler request starter.
 */
public final class ShenyuWebHandler implements WebHandler, ApplicationListener<PluginHandlerEvent> {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuWebHandler.class);

    /**
     * this filed can not set to be final, because we should copyOnWrite to update plugins.
     */
    private List<ShenyuPlugin> plugins;

    /**
     * source plugins, these plugins load from ShenyuPlugin.
     */
    private final List<ShenyuPlugin> sourcePlugins;

    /**
     * default enabled plugins.
     */
    private final Set<String> defaultEnabledPlugins = Sets.newHashSet(PluginEnum.GLOBAL.getName(),
            PluginEnum.URI.getName(), PluginEnum.WEB_CLIENT.getName(), PluginEnum.RPC_PARAM_TRANSFORM.getName());

    /**
     * enabled plugins.
     */
    private final Set<ShenyuPlugin> enabledPlugins = new CopyOnWriteArraySet<>();

    private final boolean scheduled;

    private Scheduler scheduler;

    /**
     * Instantiates a new shenyu web handler.
     *
     * @param plugins the plugins
     * @param shenyuConfig plugins config
     */
    public ShenyuWebHandler(final List<ShenyuPlugin> plugins, final ShenyuConfig shenyuConfig) {
        this.plugins = plugins;
        this.sourcePlugins = plugins;
        Set<ShenyuPlugin> defaultEnabledPlugin = this.sourcePlugins.stream()
                .filter(plugin -> defaultEnabledPlugins.contains(plugin.named()))
                .collect(Collectors.toSet());
        this.enabledPlugins.addAll(defaultEnabledPlugin);
        this.defaultEnabledPlugins.forEach(shenyuPlugin -> LOG.info("shenyu use default plugin:[{}]", shenyuPlugin));
        ShenyuConfig.Scheduler config = shenyuConfig.getScheduler();
        this.scheduled = config.getEnabled();
        if (scheduled) {
            if (Objects.equals(config.getType(), "fixed")) {
                this.scheduler = Schedulers.newParallel("shenyu-work-threads", config.getThreads());
            } else {
                this.scheduler = Schedulers.elastic();
            }
        }
    }

    /**
     * Handle the web server exchange.
     *
     * @param exchange the current server exchange
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    @Override
    public Mono<Void> handle(@NonNull final ServerWebExchange exchange) {
        Mono<Void> execute = new DefaultShenyuPluginChain(plugins).execute(exchange);
        if (scheduled) {
            return execute.subscribeOn(scheduler);
        }
        return execute;
    }

    /**
     * Put ext plugins.
     *
     * @param extPlugins the ext plugins
     */
    public void putExtPlugins(final List<ShenyuPlugin> extPlugins) {
        if (CollectionUtils.isEmpty(extPlugins)) {
            return;
        }
        List<ShenyuPlugin> shenyuPlugins = extPlugins.stream()
                .filter(e -> plugins.stream().noneMatch(plugin -> plugin.named().equals(e.named())))
                .collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(shenyuPlugins)) {
            shenyuPlugins.forEach(plugin -> LOG.info("shenyu auto add extends plugins:{}", plugin.named()));
            shenyuPlugins.addAll(plugins);
            this.plugins = sortPlugins(shenyuPlugins);
        }
    }
    
    /**
     * listen plugin handler event and handle plugin
     *
     * @param event sort plugin event
     */
    @Override
    public void onApplicationEvent(final PluginHandlerEvent event) {
        PluginHandlerEventEnums stateEnums = event.getPluginStateEnums();
        PluginData pluginData = (PluginData) event.getSource();
        switch (stateEnums) {
            case ENABLED:
                // enabled plugin.
                this.plugins = onPluginEnabled(pluginData);
                break;
            case DELETE:
            case DISABLED:
                // disable or removed plugin.
                onPluginRemoved(pluginData);
                break;
            case SORTED:
                // copy a new one, or there will be concurrency problems
                this.plugins = sortPlugins(new ArrayList<>(this.plugins));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + event.getPluginStateEnums());
        }
        this.plugins = sortPlugins(new ArrayList<>(plugins));
    }

    /**
     * sort plugins.
     *
     * @param list list of plugin
     * @return sorted list
     */
    private List<ShenyuPlugin> sortPlugins(final List<ShenyuPlugin> list) {
        Map<String, Integer> pluginSortMap = list.stream().collect(Collectors.toMap(ShenyuPlugin::named, plugin -> {
            PluginData pluginData = BaseDataCache.getInstance().obtainPluginData(plugin.named());
            return Optional.ofNullable(pluginData).map(PluginData::getSort).orElse(plugin.getOrder());
        }));
        list.sort(Comparator.comparingLong(plugin -> pluginSortMap.get(plugin.named())));
        return list;
    }

    /**
     * handle enabled plugins.
     * @param pluginData plugin data
     * @return enabled plugins
     */
    private List<ShenyuPlugin> onPluginEnabled(PluginData pluginData) {
        LOG.info("shenyu use plugin:[{}]", pluginData.getName());
        Set<ShenyuPlugin> pluginSet = this.sourcePlugins.stream().filter(plugin -> plugin.named().equals(pluginData.getName()) &&
                pluginData.getEnabled()).collect(Collectors.toSet());
        enabledPlugins.addAll(pluginSet);
        return this.sourcePlugins.stream().filter(enabledPlugins::contains).collect(Collectors.toList());
    }

    /**
     * handle removed or disabled plugin.
     * @param pluginData plugin data
     */
    private void onPluginRemoved(PluginData pluginData) {
        this.enabledPlugins.removeIf(plugin -> plugin.named().equals(pluginData.getName()));
        this.plugins.removeIf(plugin -> plugin.named().equals(pluginData.getName()));
    }

    private static class DefaultShenyuPluginChain implements ShenyuPluginChain {

        private int index;

        private final List<ShenyuPlugin> plugins;

        /**
         * Instantiates a new Default shenyu plugin chain.
         *
         * @param plugins the plugins
         */
        DefaultShenyuPluginChain(final List<ShenyuPlugin> plugins) {
            this.plugins = plugins;
        }

        /**
         * Delegate to the next {@code WebFilter} in the chain.
         *
         * @param exchange the current server exchange
         * @return {@code Mono<Void>} to indicate when request handling is complete
         */
        @Override
        public Mono<Void> execute(final ServerWebExchange exchange) {
            return Mono.defer(() -> {
                if (this.index < plugins.size()) {
                    ShenyuPlugin plugin = plugins.get(this.index++);
                    boolean skip = plugin.skip(exchange);
                    if (skip) {
                        return this.execute(exchange);
                    }
                    return plugin.execute(exchange, this);
                }
                return Mono.empty();
            });
        }
    }
}
