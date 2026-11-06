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

package org.apache.shenyu.admin.service.configs.plan;

import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.SelectorDO;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Identity resolution for selectors in a namespace import.
 */
public final class SelectorImportResolver {

    private final Map<String, Map<String, String>> existingSelectorIdByNameByPluginId;

    private final Map<String, String> pluginTemplateIdMapping;

    private final Set<String> plannedSelectorIds = new HashSet<>();

    /**
     * Create a resolver.
     *
     * @param existingSelectorIdByNameByPluginId existing selector id by plugin id and selector name
     * @param pluginTemplateIdMapping source plugin id to target plugin id mapping
     */
    public SelectorImportResolver(final Map<String, Map<String, String>> existingSelectorIdByNameByPluginId,
                                  final Map<String, String> pluginTemplateIdMapping) {
        this.existingSelectorIdByNameByPluginId = existingSelectorIdByNameByPluginId;
        this.pluginTemplateIdMapping = pluginTemplateIdMapping;
    }

    /**
     * Build a resolver from the same namespace selector query the import uses.
     *
     * @param selectorMapper selector mapper
     * @param namespace target namespace
     * @param pluginTemplateIdMapping source plugin id to target plugin id mapping
     * @return resolver
     */
    public static SelectorImportResolver from(final SelectorMapper selectorMapper,
                                              final String namespace,
                                              final Map<String, String> pluginTemplateIdMapping) {
        return from(selectorMapper.selectAllByNamespaceId(namespace), pluginTemplateIdMapping);
    }

    /**
     * Build a resolver from an already loaded namespace selector list.
     *
     * @param selectors namespace selectors
     * @param pluginTemplateIdMapping source plugin id to target plugin id mapping
     * @return resolver
     */
    public static SelectorImportResolver from(final List<SelectorDO> selectors,
                                              final Map<String, String> pluginTemplateIdMapping) {
        Map<String, Map<String, String>> existing = new LinkedHashMap<>(selectors.size());
        for (SelectorDO selector : selectors) {
            if (Objects.isNull(selector)) {
                continue;
            }
            existing.computeIfAbsent(selector.getPluginId(), key -> new LinkedHashMap<>())
                    .putIfAbsent(selector.getSelectorName(), selector.getId());
        }
        return new SelectorImportResolver(existing, pluginTemplateIdMapping);
    }

    /**
     * Resolve one source selector.
     *
     * @param selectorDTO source selector
     * @return resolution
     */
    public Resolution resolve(final SelectorDTO selectorDTO) {
        String mappedPluginId = pluginTemplateIdMapping.get(selectorDTO.getPluginId());
        if (Objects.isNull(mappedPluginId)) {
            return new Resolution(null, null, false, true);
        }
        Map<String, String> names = existingSelectorIdByNameByPluginId
                .computeIfAbsent(mappedPluginId, key -> new LinkedHashMap<>());
        if (names.containsKey(selectorDTO.getName())) {
            return new Resolution(mappedPluginId, names.get(selectorDTO.getName()), true, false);
        }
        return new Resolution(mappedPluginId, null, false, false);
    }

    /**
     * Reserve a planned or created selector for subsequent duplicate checks.
     *
     * @param selectorDTO source selector
     * @param mappedPluginId target plugin id
     * @param selectorId target selector id
     */
    public void reserve(final SelectorDTO selectorDTO, final String mappedPluginId, final String selectorId) {
        existingSelectorIdByNameByPluginId
                .computeIfAbsent(mappedPluginId, key -> new LinkedHashMap<>())
                .put(selectorDTO.getName(), selectorId);
        plannedSelectorIds.add(selectorId);
    }

    /**
     * Whether the id was reserved by this planning or import run.
     *
     * @param selectorId selector id
     * @return true when the id is planned rather than read from the database
     */
    public boolean isPlanned(final String selectorId) {
        return plannedSelectorIds.contains(selectorId);
    }

    /**
     * One selector resolution.
     *
     * @param pluginId mapped plugin id, or null when the source plugin was not mapped
     * @param existingSelectorId existing or planned selector id, or null when a new selector is needed
     * @param existing whether a selector with the same name already exists
     * @param missingPlugin whether the source plugin id has no template mapping
     */
    public record Resolution(String pluginId, String existingSelectorId, boolean existing, boolean missingPlugin) {
    }
}
