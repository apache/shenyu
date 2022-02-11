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

package org.apache.shenyu.agent.core.loader;

import org.apache.shenyu.spi.ExtensionLoader;

import java.util.Collection;

/**
 * The type Spi loader.
 */
public final class SPILoader {
    
    /**
     * Load list collection.
     *
     * @param <T> the type parameter
     * @param clazz the clazz
     * @return the collection
     */
    public static <T> Collection<T> loadList(final Class<T> clazz) {
        return ExtensionLoader.getExtensionLoader(clazz, ShenyuAgentPluginLoader.getInstance()).getJoins();
    }
    
    /**
     * Load t.
     *
     * @param <T> the type parameter
     * @param clazz the clazz
     * @param name the name
     * @return the t
     */
    public static <T> T load(final Class<T> clazz, final String name) {
        try {
            return ExtensionLoader.getExtensionLoader(clazz, ShenyuAgentPluginLoader.getInstance()).getJoin(name);
            // CHECKSTYLE:OFF
        } catch (Exception ignore) {
            // CHECKSTYLE:ON
            return null;
        }
    }
}
