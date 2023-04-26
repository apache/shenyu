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

    private final Map<String, ShenyuUploadPluginClassLoader> cache = new ConcurrentHashMap<>();

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
    public ShenyuUploadPluginClassLoader reCreate(final UploadPluginJarParser.UploadPluginJar pluginJar) {
        String jarKey = pluginJar.getJarKey();
        if (cache.containsKey(jarKey)) {
            this.remove(jarKey);
        }
        return cache.computeIfAbsent(jarKey, key -> new ShenyuUploadPluginClassLoader(pluginJar));
    }

    /**
     * get.
     *
     * @param pluginJar pluginJar
     * @return ShenyuUploadPluginClassLoader
     */
    public ShenyuUploadPluginClassLoader get(final UploadPluginJarParser.UploadPluginJar pluginJar) {
        String jarKey = pluginJar.getJarKey();
        return cache.get(jarKey);
    }

    /**
     * remove.
     *
     * @param pluginName pluginName
     */
    public void remove(final String pluginName) {
        ShenyuUploadPluginClassLoader removedLoader = cache.remove(pluginName);
        removedLoader.close();
    }

}
