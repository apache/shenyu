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
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * ShenyuPluginClassLoaderHolder.
 */
public final class ShenyuPluginClassLoaderHolder {

    private static final ShenyuPluginClassLoaderHolder HOLDER = new ShenyuPluginClassLoaderHolder();

    private final Map<String, ShenyuPluginClassLoader> pluginCache = new ConcurrentHashMap<>();

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
     * createPluginClassLoader.
     *
     * @param pluginJar pluginJar
     * @return ShenyuPluginClassLoader
     */
    public ShenyuPluginClassLoader createPluginClassLoader(final PluginJarParser.PluginJar pluginJar) {
        ShenyuPluginClassLoader shenyuPluginClassLoader = new ShenyuPluginClassLoader(pluginJar);
        String jarKey = Optional.ofNullable(pluginJar.getAbsolutePath()).orElse(pluginJar.getJarKey());
        if (pluginCache.containsKey(jarKey)) {
            pluginCache.remove(jarKey).close();
        }
        pluginCache.put(jarKey, shenyuPluginClassLoader);
        return shenyuPluginClassLoader;
    }

    /**
     * removePluginClassLoader.
     *
     * @param jarKey jarKey
     */
    public void removePluginClassLoader(final String jarKey) {
        if (pluginCache.containsKey(jarKey)) {
            pluginCache.remove(jarKey).close();
        }
    }

}
