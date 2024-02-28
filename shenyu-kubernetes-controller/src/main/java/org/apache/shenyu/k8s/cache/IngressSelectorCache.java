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

import com.google.common.collect.Maps;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

/**
 * The cache mapping Ingress to Selector id list.
 */
public final class IngressSelectorCache implements SelectorCache {

    private static final IngressSelectorCache INSTANCE = new IngressSelectorCache();

    private static final Map<String, List<String>> SELECTOR_MAP = Maps.newConcurrentMap();

    private static final AtomicLong GROWING_ID = new AtomicLong(10);

    private IngressSelectorCache() {
    }

    /**
     * Get singleton of IngressSelectorCache.
     *
     * @return IngressSelectorCache
     */
    public static IngressSelectorCache getInstance() {
        return INSTANCE;
    }

    /**
     * Put selector id list by ingress.
     *
     * @param namespace ingress namespace
     * @param name ingress name
     * @param pluginName plugin name
     * @param selectorIdList selector id list
     * @return the previous selector id list
     */
    @Override
    public List<String> put(final String namespace,
                            final String name,
                            final String pluginName,
                            final List<String> selectorIdList) {
        return SELECTOR_MAP.put(getKey(namespace, name, pluginName), selectorIdList);
    }

    /**
     * Put selector id by ingress.
     *
     * @param namespace namespace
     * @param name name
     * @param pluginName plugin name
     * @param selectorId selector id
     * @return the previous selector id list
     */
    public List<String> put(final String namespace,
                            final String name,
                            final String pluginName,
                            final String selectorId) {
        List<String> selectorIdList = SELECTOR_MAP.computeIfAbsent(getKey(namespace, name, pluginName), k -> new ArrayList<>());
        selectorIdList.add(selectorId);
        return SELECTOR_MAP.put(getKey(namespace, name, pluginName), selectorIdList);
    }

    /**
     * Get Selector id list by ingress.
     *
     * @param namespace ingress namespace
     * @param name ingress name
     * @param pluginName plugin name
     * @return selector id list
     */
    @Override
    public List<String> get(final String namespace, final String name, final String pluginName) {
        return SELECTOR_MAP.get(getKey(namespace, name, pluginName));
    }

    /**
     * Remove Selector id list by ingress.
     *
     * @param namespace ingress namespace
     * @param name ingress name
     * @param pluginName plugin name
     * @return selector id list
     */
    @Override
    public List<String> remove(final String namespace, final String name, final String pluginName) {
        return SELECTOR_MAP.remove(getKey(namespace, name, pluginName));
    }

    /**
     * Get auto-incremented selector id.
     *
     * @return selector id
     */
    public String generateSelectorId() {
        return String.valueOf(GROWING_ID.getAndIncrement());
    }

    /**
     * Get auto-incremented rule id.
     *
     * @return rule id
     */
    public String generateRuleId() {
        return String.valueOf(GROWING_ID.getAndIncrement());
    }

    /**
     * Get auto-incremented metadata id.
     *
     * @return metadata id
     */
    public String generateMetaDataId() {
        return String.valueOf(GROWING_ID.getAndIncrement());
    }

    private String getKey(final String namespace, final String name, final String pluginName) {
        return String.format("%s-%s-%s", namespace, name, pluginName);
    }
}
