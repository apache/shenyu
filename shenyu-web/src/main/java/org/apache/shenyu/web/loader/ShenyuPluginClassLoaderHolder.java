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

package org.apache.shenyu.web.loader;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Consumer;

/**
 * ShenyuPluginClassLoaderHolder.
 */
public final class ShenyuPluginClassLoaderHolder {

    private static final ShenyuPluginClassLoaderHolder HOLDER = new ShenyuPluginClassLoaderHolder();

    private final Map<String, ShenyuPluginClassLoader> pluginCache = new ConcurrentHashMap<>();

    private final Map<String, ReentrantLock> pluginLocks = new ConcurrentHashMap<>();

    private ShenyuPluginClassLoaderHolder() {
    }

    /**
     * getSingleton.
     *
     * @return ShenyuPluginClassLoaderHolder
     */
    public static ShenyuPluginClassLoaderHolder getSingleton() {
        return HOLDER;
    }

    /**
     * Load and activate a plugin before replacing its previous class loader.
     *
     * @param pluginJar pluginJar
     * @param activation plugin loading and activation callback
     */
    public void replacePluginClassLoader(final PluginJarParser.PluginJar pluginJar,
                                         final Consumer<ShenyuPluginClassLoader> activation) {
        replacePluginClassLoader(pluginJar, activation, ignored -> { });
    }

    /**
     * Replace a plugin and remove only registrations owned by the displaced loader.
     *
     * @param pluginJar plugin jar
     * @param activation loading and activation callback
     * @param deactivation ownership-aware cleanup callback
     */
    public void replacePluginClassLoader(final PluginJarParser.PluginJar pluginJar,
                                         final Consumer<ShenyuPluginClassLoader> activation,
                                         final Consumer<ShenyuPluginClassLoader> deactivation) {
        String jarKey = Optional.ofNullable(pluginJar.getAbsolutePath()).orElse(pluginJar.getJarKey());
        ReentrantLock lock = pluginLocks.computeIfAbsent(jarKey, key -> new ReentrantLock());
        lock.lock();
        ShenyuPluginClassLoader candidate = new ShenyuPluginClassLoader(pluginJar);
        ShenyuPluginClassLoader previous = pluginCache.get(jarKey);
        try {
            activation.accept(candidate);
            if (Objects.nonNull(previous)) {
                deactivation.accept(previous);
                previous.close();
            }
            pluginCache.put(jarKey, candidate);
        } catch (RuntimeException ex) {
            try {
                deactivation.accept(candidate);
                if (Objects.nonNull(previous)) {
                    activation.accept(previous);
                }
            } catch (RuntimeException rollbackFailure) {
                ex.addSuppressed(rollbackFailure);
            } finally {
                candidate.close();
            }
            throw ex;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Check whether the plugin class loader has already loaded the version.
     *
     * @param jarKey plugin jar key
     * @param version plugin version
     * @return true when the same plugin version is loaded
     */
    public boolean hasPluginClassLoader(final String jarKey, final String version) {
        ReentrantLock lock = pluginLocks.computeIfAbsent(jarKey, key -> new ReentrantLock());
        lock.lock();
        try {
            return Optional.ofNullable(pluginCache.get(jarKey))
                    .map(classLoader -> classLoader.compareVersion(version))
                    .orElse(false);
        } finally {
            lock.unlock();
        }
    }

    /**
     * removePluginClassLoader.
     *
     * @param jarKey jarKey
     * @return removed plugin names
     */
    public Set<String> removePluginClassLoader(final String jarKey) {
        return removePluginClassLoader(jarKey, ignored -> { });
    }

    /**
     * Remove a loader and its owned runtime registrations.
     *
     * @param jarKey plugin jar key
     * @param deactivation ownership-aware cleanup callback
     * @return removed plugin names
     */
    public Set<String> removePluginClassLoader(final String jarKey, final Consumer<ShenyuPluginClassLoader> deactivation) {
        ReentrantLock lock = pluginLocks.computeIfAbsent(jarKey, key -> new ReentrantLock());
        lock.lock();
        try {
            ShenyuPluginClassLoader classLoader = pluginCache.remove(jarKey);
            if (Objects.nonNull(classLoader)) {
                Set<String> pluginNames = classLoader.getLoadedPluginNames();
                deactivation.accept(classLoader);
                classLoader.close();
                return pluginNames;
            }
        } finally {
            lock.unlock();
        }
        return Collections.emptySet();
    }

}
