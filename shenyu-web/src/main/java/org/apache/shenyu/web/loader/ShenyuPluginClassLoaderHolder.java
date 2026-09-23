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

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
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
        String jarKey = Optional.ofNullable(pluginJar.getAbsolutePath()).orElse(pluginJar.getJarKey());
        ReentrantLock lock = pluginLocks.computeIfAbsent(jarKey, key -> new ReentrantLock());
        lock.lock();
        ShenyuPluginClassLoader candidate = new ShenyuPluginClassLoader(pluginJar);
        try {
            activation.accept(candidate);
            ShenyuPluginClassLoader previous = pluginCache.get(jarKey);
            if (Objects.nonNull(previous)) {
                previous.close();
            }
            pluginCache.put(jarKey, candidate);
        } catch (RuntimeException ex) {
            candidate.close();
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
     */
    public void removePluginClassLoader(final String jarKey) {
        ReentrantLock lock = pluginLocks.computeIfAbsent(jarKey, key -> new ReentrantLock());
        lock.lock();
        try {
            ShenyuPluginClassLoader classLoader = pluginCache.remove(jarKey);
            if (Objects.nonNull(classLoader)) {
                classLoader.close();
            }
        } finally {
            lock.unlock();
        }
    }

}
