/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.soul.common.extension;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The type Extension loader.
 *
 * @param <T> the type parameter
 * @author xiaoyu(Myth)
 * @author sixh.
 */
public class ExtensionLoader<T> {
    private static final String SOUL_DIRECTORY = "META-INF/soul";
    private static final Map<Class<?>, ExtensionLoader<?>> LOADRES = new ConcurrentHashMap<>();

    private final Class<T> clazz;

    private ReentrantLock lock = new ReentrantLock();

    public ExtensionLoader(Class<T> clazz) {
        this.clazz = clazz;
    }

    /**
     * Gets extension loader.
     *
     * @param <T>   the type parameter
     * @param clazz the clazz
     * @return the extension loader
     */
    @SuppressWarnings("unchecked")
    public static <T> ExtensionLoader<T> getExtensionLoader(Class<T> clazz) {
        if (clazz == null) {
            throw new NullPointerException("extension clazz is null");
        }
        if (!clazz.isInterface()) {
            throw new IllegalArgumentException("extension clazz (" + clazz + "is not interface!");
        }
        if (!clazz.isAnnotationPresent(SPI.class)) {
            throw new IllegalArgumentException("extension clazz (" + clazz + "without @" + SPI.class + "Annotation");
        }
        ExtensionLoader<T> extensionLoader = (ExtensionLoader<T>) LOADRES.get(clazz);
        if (extensionLoader != null) {
            return extensionLoader;
        }
        LOADRES.putIfAbsent(clazz, new ExtensionLoader<>(clazz));
        return (ExtensionLoader<T>) LOADRES.get(clazz);
    }

    public T getJoin() {
        return null;
    }
}
