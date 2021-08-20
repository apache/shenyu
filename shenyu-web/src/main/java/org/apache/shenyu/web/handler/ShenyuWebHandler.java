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

import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.web.loader.ShenyuPluginLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebHandler;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * This is web handler request starter.
 */
public final class ShenyuWebHandler implements WebHandler {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuWebHandler.class);

    private final List<ShenyuPlugin> plugins;
    
    private final boolean scheduled;

    private Scheduler scheduler;
    
    /**
     * Instantiates a new shenyu web handler.
     *
     * @param plugins the plugins
     */
    public ShenyuWebHandler(final List<ShenyuPlugin> plugins) {
        this.plugins = plugins;
        String enabled = System.getProperty("shenyu.scheduler.enabled", "false");
        this.scheduled = Boolean.parseBoolean(enabled);
        if (scheduled) {
            String schedulerType = System.getProperty("shenyu.scheduler.type", "fixed");
            if (Objects.equals(schedulerType, "fixed")) {
                int threads = Integer.parseInt(System.getProperty(
                        "shenyu.work.threads", "" + Math.max((Runtime.getRuntime().availableProcessors() << 1) + 1, 16)));
                scheduler = Schedulers.newParallel("shenyu-work-threads", threads);
            } else {
                scheduler = Schedulers.elastic();
            }
        }
        ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("plugin-ext-loader", true));
        executor.scheduleAtFixedRate(() -> {
            try {
                List<ShenyuPlugin> extendPlugins = ShenyuPluginLoader.getInstance().loadExtendPlugins();
                putExtPlugins(extendPlugins);
            } catch (ShenyuException | IOException | ClassNotFoundException | InstantiationException | IllegalAccessException e) {
                LOG.error("shenyu ext plugins load has error ", e);
            }
        }, 30, 300, TimeUnit.SECONDS);
    }
    
    public void putExtPlugins(final List<ShenyuPlugin> extPlugins) {
        if (CollectionUtils.isEmpty(extPlugins)) {
            return;
        }
        List<ShenyuPlugin> shenyuPlugins = extPlugins.stream()
                .filter(e -> plugins.stream().noneMatch(plugin -> plugin.named().equals(e.named())))
                .collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(shenyuPlugins)) {
            plugins.addAll(shenyuPlugins);
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
                    Boolean skip = plugin.skip(exchange);
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
