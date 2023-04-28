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
import java.util.concurrent.ConcurrentHashMap;

/**
 * ShenyuPluginClassloaderHolder.
 */
public final class ShenyuPluginClassloaderHolder {

    private static final ShenyuPluginClassloaderHolder HOLDER = new ShenyuPluginClassloaderHolder();

    private final Map<String, ShenyuPluginClassLoader> uploadPluginCache = new ConcurrentHashMap<>();

    private final Map<String, ShenyuPluginClassLoader> extPathPluginCache = new ConcurrentHashMap<>();

    private ShenyuPluginClassloaderHolder() {
    }

    /**
     * getSingleton.
     *
     * @return ShenyuPluginClassloaderHolder
     */
    public static ShenyuPluginClassloaderHolder getSingleton() {
        return HOLDER;
    }

    /**
     * reCreate.
     *
     * @param pluginJar pluginJar
     * @return ShenyuUploadPluginClassLoader
     */
    public ShenyuPluginClassLoader recreateUploadClassLoader(final PluginJarParser.PluginJar pluginJar) {
        String jarKey = pluginJar.getJarKey();
        if (uploadPluginCache.containsKey(jarKey)) {
            uploadPluginCache.remove(jarKey).close();
        }
        return uploadPluginCache.computeIfAbsent(jarKey, key -> new ShenyuPluginClassLoader(pluginJar));
    }

    /**
     * createExtPathClassLoader.
     *
     * @param pluginJar pluginJar
     * @return ShenyuPluginClassLoader
     */
    public ShenyuPluginClassLoader createExtPathClassLoader(final PluginJarParser.PluginJar pluginJar) {
        ShenyuPluginClassLoader shenyuPluginClassLoader = new ShenyuPluginClassLoader(pluginJar);
        extPathPluginCache.put(pluginJar.getAbsolutePath(), shenyuPluginClassLoader);
        return shenyuPluginClassLoader;
    }

    /**
     * get.
     *
     * @param pluginJar pluginJar
     * @return ShenyuUploadPluginClassLoader
     */
    public ShenyuPluginClassLoader getUploadClassLoader(final PluginJarParser.PluginJar pluginJar) {
        String jarKey = pluginJar.getJarKey();
        return uploadPluginCache.get(jarKey);
    }

    /**
     * removeExtPathPluginClassLoader.
     *
     * @param path path
     */
    public void removeExtPathPluginClassLoader(final String path) {
        if (extPathPluginCache.containsKey(path)) {
            extPathPluginCache.remove(path).close();
        }
    }

}
