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
import org.apache.shenyu.isolation.ReverseClassLoader;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.PluginHandlerEvent;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.web.loader.ShenyuLoaderResult;
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

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.util.*;
import java.util.stream.Collectors;

/**
 * This is web handler request starter.
 */
public final class ShenyuWebHandler implements WebHandler, ApplicationListener<PluginHandlerEvent> {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuWebHandler.class);

    private static final String PLUGIN_PATH = "./plugins/%s";

    /**
     * this filed can not set to be final, because we should copyOnWrite to update plugins.
     */
    private volatile List<ShenyuPlugin> plugins;

    /**
     * source plugins, these plugins load from ShenyuPlugin, this filed can't change.
     */
    private final List<ShenyuPlugin> sourcePlugins;

    private final ShenyuLoaderService shenyuLoaderService;

    private final boolean scheduled;

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
    public void putExtPlugins(final List<ShenyuPlugin> extPlugins) {
        if (CollectionUtils.isEmpty(extPlugins)) {
            return;
        }
        final List<ShenyuPlugin> shenyuAddPlugins = extPlugins.stream()
                .filter(e -> plugins.stream().noneMatch(plugin -> plugin.named().equals(e.named())))
                .collect(Collectors.toList());

        final List<ShenyuPlugin> shenyuUpdatePlugins = extPlugins.stream()
                .filter(e -> plugins.stream().anyMatch(plugin -> plugin.named().equals(e.named())))
                .collect(Collectors.toList());

        if (CollectionUtils.isEmpty(shenyuAddPlugins) && CollectionUtils.isEmpty(shenyuUpdatePlugins)) {
            return;
        }
        // copy new list
        List<ShenyuPlugin> newPluginList = new ArrayList<>(plugins);

        if (CollectionUtils.isNotEmpty(shenyuAddPlugins)) {
            shenyuAddPlugins.forEach(plugin -> LOG.info("shenyu auto add extends plugins:{}", plugin.named()));
            newPluginList.addAll(shenyuAddPlugins);
        }
        if (CollectionUtils.isNotEmpty(shenyuUpdatePlugins)) {
            shenyuUpdatePlugins.forEach(plugin -> LOG.info("shenyu auto update extends plugins:{}", plugin.named()));
            for (ShenyuPlugin updatePlugin : shenyuUpdatePlugins) {
                for (int i = 0; i < newPluginList.size(); i++) {
                    if (newPluginList.get(i).named().equals(updatePlugin.named())) {
                        newPluginList.set(i, updatePlugin);
                    }
                }
            }
        }
        plugins = sortPlugins(newPluginList);
    }

    /**
     * listen plugin handler event and handle plugin.
     *
     * @param event sort plugin event
     */
    @Override
    public void onApplicationEvent(final PluginHandlerEvent event) {
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
                // copy a new one, or there will be concurrency problems
                this.plugins = sortPlugins(new ArrayList<>(this.plugins));
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
    private synchronized void onPluginEnabled(final PluginData pluginData) {
        LOG.info("shenyu use plugin:[{}]", pluginData.getName());
        // SPI load plugin from the specified path
        spiLoadPlugin(pluginData);

        if (StringUtils.isNoneBlank(pluginData.getPluginJar())) {
            LOG.info("shenyu start load plugin [{}] from upload plugin jar", pluginData.getName());
            shenyuLoaderService.loadUploadedJarPlugins(pluginData.getPluginJar());
        }
        final List<ShenyuPlugin> enabledPlugins = this.sourcePlugins.stream().filter(plugin -> plugin.named().equals(pluginData.getName())
                && pluginData.getEnabled()).collect(Collectors.toList());
        enabledPlugins.removeAll(this.plugins);
        // copy a new plugin list.
        List<ShenyuPlugin> newPluginList = new ArrayList<>(this.plugins);
        newPluginList.addAll(enabledPlugins);
        this.plugins = sortPlugins(newPluginList);
    }

    private void spiLoadPlugin(final PluginData pluginData) {
        String pluginName = pluginData.getName();
        try {
            // load plugin
            String pluginJarDir = String.format(PLUGIN_PATH, pluginName);
            final File pluginJarFiles = new File(pluginJarDir);
            if (pluginJarFiles.mkdirs()) {
                return;
            }
            File[] jars = pluginJarFiles.listFiles((dir1, name) -> name.endsWith(".jar"));
            if (Objects.isNull(jars) || jars.length == 0) {
                return;
            }

            URL[] classPath = new URL[jars.length + 1];
            classPath[0] = pluginJarFiles.toURI().toURL();
            File pluginJarFile = null;
            for (int i = 1; i < classPath.length; i++) {
                final File jarFile = jars[i - 1];
                classPath[i] = jarFile.toURI().toURL();
                final String pluginJarName = String.join(Constants.DELIMITER, Constants.SHENYU, Constants.PLUGIN, pluginName);
                if (jarFile.getName().contains(pluginJarName)) {
                    pluginJarFile = jarFile;
                }
            }
            final ReverseClassLoader urlClassLoader = new ReverseClassLoader(classPath, this.getClass().getClassLoader());
            if (Objects.nonNull(pluginJarFile)) {

                final List<ShenyuLoaderResult> shenyuLoaderResults = shenyuLoaderService.loadJarPlugins(Files.newInputStream(pluginJarFile.toPath()), urlClassLoader);

                List<PluginDataHandler> handlers = shenyuLoaderResults.stream().map(ShenyuLoaderResult::getPluginDataHandler).filter(Objects::nonNull).collect(Collectors.toList());
                handlers.forEach(handler -> handler.handlerPlugin(pluginData));
            }
            LOG.info("load {} plugin success, path: {}", pluginName, pluginJarDir);
        } catch (Throwable e) {
            LOG.error("load {} plugin classloader failed. ex ", pluginName, e);
        }
    }

    /**
     * handle removed or disabled plugin.
     *
     * @param pluginData plugin data
     */
    private synchronized void onPluginRemoved(final PluginData pluginData) {
        // copy a new plugin list.
        List<ShenyuPlugin> newPluginList = new ArrayList<>(this.plugins);
        newPluginList.removeIf(plugin -> plugin.named().equals(pluginData.getName()));
        this.plugins = newPluginList;
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
                    plugin.before(exchange);
                    Mono<Void> execute = plugin.execute(exchange, this);
                    plugin.after(exchange);
                    return execute;
                }
                return Mono.empty();
            });
        }
    }
}
