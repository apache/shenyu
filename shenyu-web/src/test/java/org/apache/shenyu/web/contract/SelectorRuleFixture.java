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

package org.apache.shenyu.web.contract;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.GenericApplicationContext;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Owns isolated plugin, selector, rule and match-cache buckets for selector/rule tests.
 *
 * <p>Each plugin must first be claimed through {@link #cachePluginData(PluginData)} with an
 * unused name. Selector IDs must also be unused because rule buckets are keyed globally by
 * selector ID. Only entries owned by this fixture are cleared; existing entries are never
 * replaced or restored from snapshots.</p>
 *
 * <p>The fixture owns a small Spring context because matching reads {@link ShenyuConfig}
 * through {@link SpringBeanUtils}. A reentrant lock serializes fixture lifetimes. Nested
 * fixtures with different plugin names and selector IDs are supported, but every fixture
 * must be used and closed on its creating thread, and nested fixtures must close in LIFO
 * order. Other tests accessing these global caches or SpringBeanUtils must also run serially;
 * the lock cannot coordinate code that does not use this fixture.</p>
 */
public final class SelectorRuleFixture implements AutoCloseable {

    private static final ReentrantLock LIFETIME_LOCK = new ReentrantLock();

    private static final Deque<SelectorRuleFixture> ACTIVE_FIXTURES = new ArrayDeque<>();

    private final Set<String> ownedPluginNames = new HashSet<>();

    private final Map<String, String> ownedSelectors = new HashMap<>();

    private final ApplicationContext previousContext;

    private final GenericApplicationContext context;

    private final Thread ownerThread;

    private boolean closed;

    /**
     * Creates and installs the fixture context without changing existing cache entries.
     */
    public SelectorRuleFixture() {
        LIFETIME_LOCK.lock();
        this.ownerThread = Thread.currentThread();
        this.previousContext = SpringBeanUtils.getInstance().getApplicationContext();
        this.context = new GenericApplicationContext();
        boolean installed = false;
        try {
            this.context.registerBean(ShenyuConfig.class, ShenyuConfig::new);
            this.context.refresh();
            SpringBeanUtils.getInstance().setApplicationContext(this.context);
            ACTIVE_FIXTURES.push(this);
            installed = true;
        } finally {
            if (!installed) {
                try {
                    SpringBeanUtils.getInstance().setApplicationContext(previousContext);
                    context.close();
                } finally {
                    LIFETIME_LOCK.unlock();
                }
            }
        }
    }

    /**
     * Claims an unused plugin name, or updates a definition already owned by this fixture.
     *
     * @param pluginData plugin data
     */
    public void cachePluginData(final PluginData pluginData) {
        requireActive();
        final String pluginName = requireName(Objects.requireNonNull(pluginData, "pluginData").getName());
        if (!ownedPluginNames.contains(pluginName)) {
            final BaseDataCache cache = BaseDataCache.getInstance();
            final MatchDataCache matchCache = MatchDataCache.getInstance();
            if (ACTIVE_FIXTURES.stream().anyMatch(fixture -> fixture.ownedPluginNames.contains(pluginName))
                    || cache.getPluginMap().containsKey(pluginName) || cache.getSelectorMap().containsKey(pluginName)
                    || matchCache.getSelectorMatchCache().containsKey(pluginName) || matchCache.getRuleMatchCache().containsKey(pluginName)) {
                throw new IllegalArgumentException("Plugin cache buckets already exist: " + pluginName);
            }
            ownedPluginNames.add(pluginName);
        }
        BaseDataCache.getInstance().cachePluginData(pluginData);
        MatchDataCache.getInstance().removeSelectorData(pluginName);
        MatchDataCache.getInstance().removeRuleData(pluginName);
    }

    /**
     * Caches a selector definition for a claimed plugin, claiming its unused selector ID.
     *
     * @param selectorData selector data
     */
    public void cacheSelectorData(final SelectorData selectorData) {
        requireActive();
        Objects.requireNonNull(selectorData, "selectorData");
        final String pluginName = requireOwnedPlugin(selectorData.getPluginName());
        final String selectorId = requireName(selectorData.getId());
        if (!ownedSelectors.containsKey(selectorId)) {
            final BaseDataCache cache = BaseDataCache.getInstance();
            if (ACTIVE_FIXTURES.stream().anyMatch(fixture -> fixture.ownedSelectors.containsKey(selectorId))
                    || cache.getRuleMap().containsKey(selectorId)
                    || cache.getSelectorMap().values().stream().flatMap(Collection::stream)
                    .anyMatch(selector -> selectorId.equals(selector.getId()))) {
                throw new IllegalArgumentException("Selector cache buckets already exist: " + selectorId);
            }
            ownedSelectors.put(selectorId, pluginName);
        }
        requireOwnedSelector(pluginName, selectorId);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        invalidateSelector(selectorData);
    }

    /**
     * Caches a rule definition for a claimed plugin and an owned selector.
     *
     * @param ruleData rule data
     */
    public void cacheRuleData(final RuleData ruleData) {
        requireActive();
        Objects.requireNonNull(ruleData, "ruleData");
        requireOwnedSelector(requireOwnedPlugin(ruleData.getPluginName()), ruleData.getSelectorId());
        requireName(ruleData.getId());
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        invalidateRule(ruleData);
    }

    /**
     * Removes an owned plugin definition, retaining ownership until close.
     *
     * @param pluginName plugin name
     */
    public void removePluginData(final String pluginName) {
        requireActive();
        requireOwnedPlugin(pluginName);
        BaseDataCache.getInstance().removePluginDataByPluginName(pluginName);
        MatchDataCache.getInstance().removeSelectorData(pluginName);
        MatchDataCache.getInstance().removeRuleData(pluginName);
    }

    /**
     * Removes an owned selector definition and invalidates its selector and rule matches.
     *
     * @param selectorData selector data
     */
    public void removeSelectorData(final SelectorData selectorData) {
        requireActive();
        Objects.requireNonNull(selectorData, "selectorData");
        requireOwnedSelector(requireOwnedPlugin(selectorData.getPluginName()), selectorData.getId());
        BaseDataCache.getInstance().removeSelectData(selectorData);
        invalidateSelector(selectorData);
    }

    /**
     * Removes a rule definition belonging to an owned selector and invalidates its matches.
     *
     * @param ruleData rule data
     */
    public void removeRuleData(final RuleData ruleData) {
        requireActive();
        Objects.requireNonNull(ruleData, "ruleData");
        requireOwnedSelector(requireOwnedPlugin(ruleData.getPluginName()), ruleData.getSelectorId());
        requireName(ruleData.getId());
        BaseDataCache.getInstance().removeRuleData(ruleData);
        invalidateRule(ruleData);
    }

    /**
     * Clears only owned cache buckets, retaining ownership until close.
     */
    public void clear() {
        requireActive();
        final BaseDataCache cache = BaseDataCache.getInstance();
        ownedSelectors.keySet().forEach(cache::removeRuleDataBySelectorId);
        for (String pluginName : ownedPluginNames) {
            cache.removePluginDataByPluginName(pluginName);
            cache.removeSelectDataByPluginName(pluginName);
            MatchDataCache.getInstance().removeSelectorData(pluginName);
            MatchDataCache.getInstance().removeRuleData(pluginName);
        }
    }

    @Override
    public void close() {
        requireOwnerThread();
        if (closed) {
            return;
        }
        requireActive();
        if (ACTIVE_FIXTURES.peek() != this) {
            throw new IllegalStateException("Nested fixtures must close in LIFO order");
        }
        try {
            clear();
        } finally {
            SpringBeanUtils.getInstance().setApplicationContext(previousContext);
            try {
                context.close();
            } finally {
                closed = true;
                ACTIVE_FIXTURES.pop();
                LIFETIME_LOCK.unlock();
            }
        }
    }

    private void invalidateSelector(final SelectorData selectorData) {
        final MatchDataCache cache = MatchDataCache.getInstance();
        // A new or reprioritized selector may displace any previously cached positive match.
        cache.removeSelectorData(selectorData.getPluginName());
        cache.removeRuleData(selectorData.getPluginName());
    }

    private void invalidateRule(final RuleData ruleData) {
        final MatchDataCache cache = MatchDataCache.getInstance();
        // Rules compete by priority, so invalidating only the changed rule ID is insufficient.
        cache.removeRuleData(ruleData.getPluginName());
    }

    private String requireOwnedPlugin(final String pluginName) {
        requireName(pluginName);
        if (!ownedPluginNames.contains(pluginName)) {
            throw new IllegalArgumentException("Plugin is not owned by this fixture: " + pluginName);
        }
        return pluginName;
    }

    private void requireOwnedSelector(final String pluginName, final String selectorId) {
        requireName(selectorId);
        if (!pluginName.equals(ownedSelectors.get(selectorId))) {
            throw new IllegalArgumentException("Selector is not owned by this plugin: " + selectorId);
        }
    }

    private String requireName(final String name) {
        if (Objects.isNull(name) || name.isBlank()) {
            throw new IllegalArgumentException("Cache names and IDs must not be blank");
        }
        return name;
    }

    private void requireActive() {
        requireOwnerThread();
        if (closed) {
            throw new IllegalStateException("Fixture is closed");
        }
    }

    private void requireOwnerThread() {
        if (Thread.currentThread() != ownerThread) {
            throw new IllegalStateException("Fixture must be used and closed on its creating thread");
        }
    }
}
