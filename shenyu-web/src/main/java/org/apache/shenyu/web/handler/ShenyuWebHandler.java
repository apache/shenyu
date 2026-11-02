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
import org.apache.shenyu.common.metrics.PluginObservationEvent;
import org.apache.shenyu.plugin.api.observation.PluginObservationContext;
import org.apache.shenyu.web.observation.PluginObservationManager;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.TraceIdUtils;
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
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * This is web handler request starter.
 */
public final class ShenyuWebHandler implements WebHandler, ApplicationListener<PluginHandlerEvent> {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuWebHandler.class);

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

    private final PluginObservationManager observations;

    private Scheduler scheduler;
    
    /**
     * Instantiates a new shenyu web handler.
     *
     * @param plugins the plugins
     * @param shenyuLoaderService shenyuLoaderService
     * @param shenyuConfig plugins config
     */
    public ShenyuWebHandler(final List<ShenyuPlugin> plugins, final ShenyuLoaderService shenyuLoaderService, final ShenyuConfig shenyuConfig) {
        this(plugins, shenyuLoaderService, shenyuConfig, PluginObservationManager.disabled());
    }

    /**
     * Create an optionally observed legacy handler without changing plugin lifecycle hooks.
     * @param plugins ordered implementations
     * @param shenyuLoaderService loader
     * @param shenyuConfig gateway configuration
     * @param observations optional bounded observation owner
     */
    public ShenyuWebHandler(final List<ShenyuPlugin> plugins, final ShenyuLoaderService shenyuLoaderService, final ShenyuConfig shenyuConfig,
                           final PluginObservationManager observations) {
        this.observations = observations;
        this.sourcePlugins = new ArrayList<>(plugins);
        this.plugins = new ArrayList<>(plugins);
        observations.prepare(this.plugins);
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
     * Start monotonic chain timing for a subscription, before any scheduler queueing.
     *
     * @param exchange context
     */
    public void before(final ServerWebExchange exchange) {
        exchange.getAttributes().put(Constants.CHAIN_START_TIME, System.nanoTime());
    }

    /**
     * Chain after operation.
     *
     * <p>It runs with the terminal signal of the request, not while the chain is assembled, so the
     * reported cost is the duration of the request and the trace id of the gateway span is already
     * populated. A long lived response, for example a streamed one, reports its timing once the
     * stream has ended. Each subscription starts its own timing; repeated cleanup of the same
     * subscription is ignored. The cost is reported in milliseconds.</p>
     *
     * @param exchange context
     */
    public void after(final ServerWebExchange exchange) {
        Map<String, Object> attributes = exchange.getAttributes();
        Object startTime = attributes.remove(Constants.CHAIN_START_TIME);
        if (Objects.isNull(startTime)) {
            return;
        }
        long cost = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - (long) startTime);
        LOG.debug("shenyu chain handle uri:{}, traceId:{}, cost:{}", exchange.getRequest().getPath(), TraceIdUtils.traceId(exchange), cost);
    }

    /**
     * Handle the web server exchange.
     *
     * <p>Timing includes scheduler queueing and asynchronous execution until completion, failure
     * or cancellation. Creating the publisher does not start timing. Each subscription creates
     * a fresh chain, so sequential subscriptions execute all plugins again.</p>
     *
     * @param exchange the current server exchange
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    @Override
    public Mono<Void> handle(@NonNull final ServerWebExchange exchange) {
        return Mono.defer(() -> {
            before(exchange);
            Mono<Void> execute = new DefaultShenyuPluginChain(plugins, observations.start()).execute(exchange);
            if (scheduled) {
                execute = execute.subscribeOn(scheduler);
            }
            return execute.doFinally(signalType -> after(exchange));
        });
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
     * Inspect bounded exporter/drop state without reading request data.
     * @return observation diagnostics
     */
    public PluginObservationManager.Stats observationStats() {
        return observations.stats();
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
        
        // Add extend plugin from pluginData or shenyu ext-lib
        this.sourcePlugins.addAll(shenyuAddPlugins);

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
                for (int i = 0; i < this.sourcePlugins.size(); i++) {
                    if (this.sourcePlugins.get(i).named().equals(updatePlugin.named())) {
                        this.sourcePlugins.set(i, updatePlugin);
                    }
                }
            }
        }
        plugins = sortPlugins(newPluginList);
    }

    /**
     * Remove only the plugin instances owned by a displaced extension loader.
     *
     * @param classLoader owner class loader
     */
    public synchronized void removeExtPluginsByClassLoader(final ClassLoader classLoader) {
        sourcePlugins.removeIf(plugin -> plugin.getClass().getClassLoader() == classLoader);
        plugins = plugins.stream().filter(plugin -> plugin.getClass().getClassLoader() != classLoader).collect(Collectors.toList());
    }

    /**
     * Remove ext plugins.
     *
     * @param pluginNames plugin names
     */
    public synchronized void removeExtPlugins(final Set<String> pluginNames) {
        if (CollectionUtils.isEmpty(pluginNames)) {
            return;
        }
        pluginNames.forEach(pluginName -> LOG.info("shenyu auto remove extends plugin:{}", pluginName));
        sourcePlugins.removeIf(plugin -> pluginNames.contains(plugin.named()));
        plugins = plugins.stream().filter(plugin -> !pluginNames.contains(plugin.named())).collect(Collectors.toList());
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
                observations.prepare(this.plugins);
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
        if (StringUtils.isNoneBlank(pluginData.getPluginJar())) {
            LOG.info("shenyu start load plugin [{}] from upload plugin jar", pluginData.getName());
            shenyuLoaderService.loadExtOrUploadPlugins(pluginData);
        }
        final List<ShenyuPlugin> enabledPlugins = this.sourcePlugins.stream().filter(plugin -> plugin.named().equals(pluginData.getName())
                && pluginData.getEnabled()).collect(Collectors.toList());
        enabledPlugins.removeAll(this.plugins);
        // copy a new plugin list.
        List<ShenyuPlugin> newPluginList = new ArrayList<>(this.plugins);
        newPluginList.addAll(enabledPlugins);
        this.plugins = sortPlugins(newPluginList);
        observations.prepare(this.plugins);
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
        observations.prepare(this.plugins);
    }

    private static class DefaultShenyuPluginChain implements ShenyuPluginChain {

        private int index;

        private final List<ShenyuPlugin> plugins;

        private final PluginObservationManager.Scope observation;
    
        /**
         * Instantiates a new Default shenyu plugin chain.
         *
         * @param plugins the plugins
         */
        DefaultShenyuPluginChain(final List<ShenyuPlugin> plugins) {
            this(plugins, null);
        }

        DefaultShenyuPluginChain(final List<ShenyuPlugin> plugins, final PluginObservationManager.Scope observation) {
            this.plugins = plugins;
            this.observation = observation;
        }

        @Override
        public ShenyuPluginChain fork() {
            return new DefaultShenyuPluginChain(plugins, observation);
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
                    if (Objects.nonNull(observation)) {
                        return observe(plugin, exchange);
                    }
                    boolean skip = plugin.skip(exchange);
                    if (skip) {
                        return this.execute(exchange);
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

        private Mono<Void> observe(final ShenyuPlugin plugin, final ServerWebExchange exchange) {
            Object trace = exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID);
            PluginObservationManager.Span span = observation.span(plugin, trace instanceof String ? (String) trace : "");
            final boolean skip;
            try {
                skip = plugin.skip(exchange);
            } catch (RuntimeException | AssertionError | LinkageError failure) {
                span.constructed(PluginObservationEvent.Kind.SKIP_ERROR);
                throw failure;
            }
            if (skip) {
                span.constructed(PluginObservationEvent.Kind.SKIPPED);
                return execute(exchange);
            }
            Object previous = exchange.getAttributes().put(PluginObservationContext.KEY, span);
            try {
                final Mono<Void> result;
                try {
                    plugin.before(exchange);
                    result = plugin.execute(exchange, this);
                } finally {
                    plugin.after(exchange);
                }
                span.constructed(PluginObservationEvent.Kind.CONSTRUCTED);
                return span.publisher(result);
            } catch (RuntimeException | AssertionError | LinkageError failure) {
                span.constructed(PluginObservationEvent.Kind.CONSTRUCTION_ERROR);
                throw failure;
            } finally {
                if (Objects.isNull(previous)) {
                    exchange.getAttributes().remove(PluginObservationContext.KEY);
                } else {
                    exchange.getAttributes().put(PluginObservationContext.KEY, previous);
                }
            }
        }
    }
}
