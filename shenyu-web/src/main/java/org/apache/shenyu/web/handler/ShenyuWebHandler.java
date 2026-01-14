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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginHandlerEventEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.PluginHandlerEvent;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebHandler;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * This is web handler request starter.
 */
public final class ShenyuWebHandler implements WebHandler, ApplicationListener<PluginHandlerEvent> {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuWebHandler.class);

    /**
     * this field uses volatile List for thread-safe operations.
     */
    private volatile List<ShenyuPlugin> plugins;

    /**
     * source plugins, these plugins load from ShenyuPlugin, this filed can't change.
     */
    private final List<ShenyuPlugin> sourcePlugins;

    private final ShenyuLoaderService shenyuLoaderService;

    private final boolean scheduled;

    private final ShenyuConfig shenyuConfig;

    private Scheduler scheduler;

    /**
     * Instantiates a new shenyu web handler.
     *
     * @param plugins the plugins
     * @param shenyuLoaderService shenyuLoaderService
     * @param shenyuConfig plugins config
     */
    public ShenyuWebHandler(final List<ShenyuPlugin> plugins, final ShenyuLoaderService shenyuLoaderService, final ShenyuConfig shenyuConfig) {
        this.sourcePlugins = new ArrayList<>(plugins);
        this.plugins = new ArrayList<>(plugins);
        this.shenyuLoaderService = shenyuLoaderService;
        this.shenyuConfig = shenyuConfig;
        ShenyuConfig.Scheduler config = shenyuConfig.getScheduler();
        this.scheduled = config.getEnabled();
        if (scheduled) {
            if (Objects.equals(config.getType(), "fixed")) {
                this.scheduler = Schedulers.newParallel("shenyu-work-threads", config.getThreads());
            } else {
                this.scheduler = Schedulers.boundedElastic();
            }
        }
    }

    /**
     * Chain before operation.
     *
     * @param exchange context
     */
    public void before(final ServerWebExchange exchange) {
        exchange.getAttributes().put(Constants.CHAIN_START_TIME, System.currentTimeMillis());
        exchange.getAttributes().put(Constants.LOGGING_ENABLED, shenyuConfig.getPlugin().getEnabled());
        exchange.getAttributes().put(Constants.LOGGING_MIN_COST, shenyuConfig.getPlugin().getMinCost());
    }

    /**
     * Plugin after operation.
     *
     * @param exchange context
     */
    public void after(final ServerWebExchange exchange) {
        Map<String, Object> attributes = exchange.getAttributes();
        long currentTimeMillis = System.currentTimeMillis();
        long startTime = (long) attributes.get(Constants.CHAIN_START_TIME);
        LOG.debug("shenyu chain handle uri:{}, traceId:{}, cost:{}", exchange.getRequest().getPath(), exchange.getLogPrefix(), currentTimeMillis - startTime);
        attributes.remove(Constants.CHAIN_START_TIME);
    }

    /**
     * Handle the web server exchange.
     *
     * @param exchange the current server exchange
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    @Override
    public Mono<Void> handle(@NonNull final ServerWebExchange exchange) {
        return Mono.defer(() -> {
            before(exchange);
            return new DefaultShenyuPluginChain(plugins).execute(exchange);
        }).doOnError(Throwable.class, e -> LOG.error("shenyu execute plugin exception: ", e))
          .doFinally(signalType -> after(exchange))
          .subscribeOn(scheduled ? scheduler : Schedulers.immediate());
    }
    
    /**
     * Gets plugins.
     *
     * @return the plugins
     */
    public List<ShenyuPlugin> getPlugins() {
        return plugins;
    }
    
    /**
     * Put ext plugins.
     *
     * @param extPlugins the ext plugins
     */
    public synchronized void putExtPlugins(final List<ShenyuPlugin> extPlugins) {
        if (CollectionUtils.isEmpty(extPlugins)) {
            return;
        }

        // Copy-on-write: create new list based on current volatile list
        List<ShenyuPlugin> newPlugins = new ArrayList<>(this.plugins);

        // Create a map of existing plugins for O(1) lookup
        Map<String, ShenyuPlugin> existingPluginMap = newPlugins.stream()
                .collect(Collectors.toMap(ShenyuPlugin::named, Function.identity()));

        boolean hasAdditions = false;
        boolean hasUpdates = false;

        // Single pass through extPlugins to handle both additions and updates
        for (ShenyuPlugin extPlugin : extPlugins) {
            String pluginName = extPlugin.named();
            if (existingPluginMap.containsKey(pluginName)) {
                // Update existing plugin
                updatePluginInList(extPlugin, newPlugins);
                updatePluginInSource(extPlugin);
                hasUpdates = true;
                LOG.info("shenyu auto update extends plugins:{}", pluginName);
            } else {
                // Add new plugin
                newPlugins.add(extPlugin);
                this.sourcePlugins.add(extPlugin);
                hasAdditions = true;
                LOG.info("shenyu auto add extends plugins:{}", pluginName);
            }
        }

        if (hasAdditions || hasUpdates) {
            // Re-sort the plugins list
            List<ShenyuPlugin> sortedPlugins = sortPlugins(newPlugins);
            // Volatile write
            this.plugins = sortedPlugins;
        }
    }

    /**
     * Update plugin in the given plugin list.
     *
     * @param updatePlugin the plugin to update
     * @param pluginList the plugin list to update
     */
    private void updatePluginInList(final ShenyuPlugin updatePlugin, final List<ShenyuPlugin> pluginList) {
        for (int i = 0; i < pluginList.size(); i++) {
            if (pluginList.get(i).named().equals(updatePlugin.named())) {
                pluginList.set(i, updatePlugin);
                break;
            }
        }
    }

    /**
     * Update plugin in the source plugins list.
     *
     * @param updatePlugin the plugin to update
     */
    private void updatePluginInSource(final ShenyuPlugin updatePlugin) {
        for (int i = 0; i < this.sourcePlugins.size(); i++) {
            if (this.sourcePlugins.get(i).named().equals(updatePlugin.named())) {
                this.sourcePlugins.set(i, updatePlugin);
                break;
            }
        }
    }

    /**
     * listen plugin handler event and handle plugin.
     *
     * @param event sort plugin event
     */
    @Override
    public synchronized void onApplicationEvent(final PluginHandlerEvent event) {
        PluginHandlerEventEnum stateEnums = event.getPluginStateEnums();
        PluginData pluginData = (PluginData) event.getSource();
        switch (stateEnums) {
            case ENABLED:
                onPluginEnabled(pluginData);
                break;
            case DELETE:
            case DISABLED:
                // disable or removed plugin.
                onPluginRemoved(pluginData);
                break;
            case SORTED:
                // Re-sort the plugins list
                // Copy-on-write
                List<ShenyuPlugin> newPlugins = new ArrayList<>(this.plugins);
                List<ShenyuPlugin> sortedPlugins = sortPlugins(newPlugins);
                // Volatile write
                this.plugins = sortedPlugins;
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + event.getPluginStateEnums());
        }
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
     *
     * @param pluginData plugin data
     */
    private void onPluginEnabled(final PluginData pluginData) {
        LOG.info("shenyu use plugin:[{}]", pluginData.getName());
        if (StringUtils.isNoneBlank(pluginData.getPluginJar())) {
            LOG.info("shenyu start load plugin [{}] from upload plugin jar", pluginData.getName());
            shenyuLoaderService.loadExtOrUploadPlugins(pluginData);
        }
        final List<ShenyuPlugin> enabledPlugins = this.sourcePlugins.stream().filter(plugin -> plugin.named().equals(pluginData.getName())
                && pluginData.getEnabled()).collect(Collectors.toList());

        // Copy-on-write
        List<ShenyuPlugin> newPlugins = new ArrayList<>(this.plugins);
        enabledPlugins.removeAll(newPlugins);
        if (!enabledPlugins.isEmpty()) {
            // Add enabled plugins
            newPlugins.addAll(enabledPlugins);
            // Re-sort the plugins list
            List<ShenyuPlugin> sortedPlugins = sortPlugins(newPlugins);
            // Volatile write
            this.plugins = sortedPlugins;
        }
    }

    /**
     * handle removed or disabled plugin.
     *
     * @param pluginData plugin data
     */
    private void onPluginRemoved(final PluginData pluginData) {
        // Copy-on-write
        List<ShenyuPlugin> newPlugins = new ArrayList<>(this.plugins);
        // Remove plugin from plugins list
        newPlugins.removeIf(plugin -> plugin.named().equals(pluginData.getName()));
        // Volatile write
        this.plugins = newPlugins;
    }

    private static class DefaultShenyuPluginChain implements ShenyuPluginChain {

        private final AtomicInteger index = new AtomicInteger(0);

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
                int pos;
                while ((pos = index.getAndIncrement()) < plugins.size()) {
                    ShenyuPlugin plugin = plugins.get(pos);
                    if (plugin.skip(exchange)) {
                        continue;
                    }
                    try {
                        plugin.before(exchange);
                        return plugin.execute(exchange, this);
                    } finally {
                        plugin.after(exchange);
                    }
                }
                return Mono.empty();
            });
        }
    }
}
