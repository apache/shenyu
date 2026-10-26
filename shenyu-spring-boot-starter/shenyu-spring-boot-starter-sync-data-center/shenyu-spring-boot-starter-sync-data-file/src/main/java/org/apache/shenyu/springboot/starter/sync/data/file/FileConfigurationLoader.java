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

package org.apache.shenyu.springboot.starter.sync.data.file;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.InitialSyncApplication;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamKey;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.springframework.beans.factory.SmartInitializingSingleton;
import org.springframework.boot.convert.DurationStyle;
import org.springframework.core.env.Environment;

import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * Applies a validated snapshot before server startup and tracks asynchronous handler completion.
 */
public final class FileConfigurationLoader implements SmartInitializingSingleton, AutoCloseable {

    private final Environment environment;

    private final FileConfigurationParser parser;

    private final PluginDataSubscriber subscriber;

    private final DiscoveryUpstreamDataSubscriber upstreamSubscriber;

    private FileConfigurationParser.Snapshot snapshot;

    private volatile boolean ready;

    private String applying = "configuration";

    FileConfigurationLoader(final Environment environment, final String namespace, final List<ShenyuPlugin> plugins,
                            final List<PluginDataHandler> handlers, final List<DiscoveryUpstreamDataHandler> upstreamHandlers,
                            final PluginDataSubscriber subscriber, final DiscoveryUpstreamDataSubscriber upstreamSubscriber) {
        this.environment = environment;
        this.subscriber = subscriber;
        this.upstreamSubscriber = upstreamSubscriber;
        this.parser = new FileConfigurationParser(environment, namespace, plugins, handlers.stream().map(PluginDataHandler::pluginNamed).collect(Collectors.toSet()),
                upstreamHandlers.stream().map(DiscoveryUpstreamDataHandler::pluginName).collect(Collectors.toSet()));
    }

    @Override
    public void afterSingletonsInstantiated() {
        String location = environment.getProperty("shenyu.sync.file.path", "");
        if (location.isBlank()) {
            throw new IllegalStateException("File configuration requires shenyu.sync.file.path to an external YAML/JSON file");
        }
        Duration timeout;
        try {
            timeout = DurationStyle.detectAndParse(environment.getProperty("shenyu.sync.file.apply-timeout", "30s"));
            if (timeout.compareTo(Duration.ofMillis(1)) < 0 || timeout.compareTo(Duration.ofMinutes(5)) > 0) {
                throw new IllegalArgumentException();
            }
        } catch (IllegalArgumentException ex) {
            throw new IllegalStateException("shenyu.sync.file.apply-timeout must be between 1ms and 5m");
        }
        Path path;
        try {
            path = Path.of(location);
        } catch (IllegalArgumentException ex) {
            throw new IllegalStateException("shenyu.sync.file.path must be a local filesystem path");
        }
        snapshot = parser.read(path);
        try {
            InitialSyncApplication.run(this::apply).get(timeout.toMillis(), TimeUnit.MILLISECONDS);
            ready = true;
        } catch (Exception ex) {
            if (ex instanceof InterruptedException) {
                Thread.currentThread().interrupt();
            }
            close();
            // Subscriber causes can contain credentials. Report the application stage without the payload/cause.
            throw new IllegalStateException("File configuration application failed at " + applying + " (" + ex.getClass().getSimpleName() + ")");
        }
    }

    private void apply() {
        publish("plugins", snapshot.plugins(), subscriber::onSubscribe);
        publish("selectors", snapshot.selectors(), subscriber::onSelectorSubscribe);
        publish("rules", snapshot.rules(), subscriber::onRuleSubscribe);
        publish("upstreams", snapshot.upstreams(), upstreamSubscriber::onSubscribe);
        applying = "asynchronous handlers";
    }

    private <T> void publish(final String section, final List<T> entries, final Consumer<T> action) {
        for (int index = 0; index < entries.size(); index++) {
            applying = section + "[" + index + "]";
            action.accept(entries.get(index));
        }
    }

    /**
     * Whether all startup configuration has been applied successfully.
     * @return readiness
     */
    public boolean isReady() {
        return ready;
    }

    @Override
    public void close() {
        ready = false;
        if (Objects.isNull(snapshot)) {
            return;
        }
        // Cleanup each entry independently, including entries whose handler failed after updating a cache.
        for (DiscoverySyncData upstream : snapshot.upstreams()) {
            cleanup(() -> upstreamSubscriber.unSubscribe(DiscoveryUpstreamKey.from(upstream)));
        }
        for (RuleData rule : snapshot.rules()) {
            cleanup(() -> subscriber.unRuleSubscribe(rule));
        }
        for (SelectorData selector : snapshot.selectors()) {
            cleanup(() -> subscriber.unSelectorSubscribe(selector));
        }
        for (PluginData plugin : snapshot.plugins()) {
            cleanup(() -> subscriber.unSubscribe(plugin));
        }
        snapshot = null;
    }

    private void cleanup(final Runnable action) {
        try {
            InitialSyncApplication.run(action).orTimeout(5, TimeUnit.SECONDS).join();
        } catch (RuntimeException ignored) {
            // The failed context never becomes ready. Continue removing the remaining owned entries.
        }
    }
}
