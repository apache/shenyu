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

package org.apache.shenyu.k8s.cache;

import java.util.List;

/**
 * The cache for Selector id.
 */
public interface SelectorCache {

    /**
     * Put Selector id list by resource namespace, name and plugin name.
     *
     * @param namespace resource namespace
     * @param name resource name
     * @param pluginName plugin name
     * @param selectorIdList selector id list
     * @return The previous value associated with key, or null if there was no mapping for key
     */
    List<String> put(String namespace, String name, String pluginName, List<String> selectorIdList);

    /**
     * Get Selector id list by resource namespace, name and plugin name.
     *
     * @param namespace resource namespace
     * @param name resource name
     * @param pluginName plugin name
     * @return Selector id list
     */
    List<String> get(String namespace, String name, String pluginName);

    /**
     * Remove Selector id list by resource namespace, name and plugin name.
     *
     * @param namespace resource namespace
     * @param name resource name
     * @param pluginName plugin name
     * @return Selector id list
     */
    List<String> remove(String namespace, String name, String pluginName);
}
