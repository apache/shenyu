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

import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Identity resolution for the global plugin template section.
 */
public final class PluginTemplateImportResolver {

    private final Map<String, String> existingPluginIdByName;

    private final Map<String, String> pluginTemplateIdMapping;

    /**
     * Create a resolver.
     *
     * @param existingPluginIdByName existing global plugin id by name
     * @param pluginTemplateIdMapping source plugin id to target plugin id mapping
     */
    public PluginTemplateImportResolver(final Map<String, String> existingPluginIdByName,
                                        final Map<String, String> pluginTemplateIdMapping) {
        this.existingPluginIdByName = existingPluginIdByName;
        this.pluginTemplateIdMapping = pluginTemplateIdMapping;
    }

    /**
     * Build a resolver from the same global plugin query the import uses.
     *
     * @param pluginMapper plugin mapper
     * @param pluginTemplateIdMapping source plugin id to target plugin id mapping
     * @return resolver
     */
    public static PluginTemplateImportResolver from(final PluginMapper pluginMapper,
                                                    final Map<String, String> pluginTemplateIdMapping) {
        Map<String, String> existing = new LinkedHashMap<>();
        for (PluginDO plugin : pluginMapper.selectAll()) {
            if (Objects.isNull(plugin)) {
                continue;
            }
            if (existing.containsKey(plugin.getName())) {
                throw new IllegalStateException("Duplicate plugin name: " + plugin.getName());
            }
            existing.put(plugin.getName(), plugin.getId());
        }
        return new PluginTemplateImportResolver(existing, pluginTemplateIdMapping);
    }

    /**
     * Resolve one source plugin template.
     *
     * @param pluginDTO source plugin
     * @return decision; the plugin template id mapping is populated as a side effect
     */
    public ConfigImportDecision resolve(final PluginDTO pluginDTO) {
        if (existingPluginIdByName.containsKey(pluginDTO.getName())) {
            String existingId = existingPluginIdByName.get(pluginDTO.getName());
            String targetRef = Objects.isNull(existingId) ? "existing" : "existing:" + existingId;
            pluginTemplateIdMapping.put(pluginDTO.getId(), existingId);
            return new ConfigImportDecision(pluginDTO.getId(), pluginDTO.getName(), ConfigImportAction.REUSE,
                    existingId, targetRef,
                    List.of("plugin:" + pluginDTO.getId() + " -> " + targetRef), Collections.emptyList());
        }
        String newId = UUIDUtils.getInstance().generateShortUuid();
        pluginTemplateIdMapping.put(pluginDTO.getId(), newId);
        return new ConfigImportDecision(pluginDTO.getId(), pluginDTO.getName(), ConfigImportAction.CREATE,
                newId, "new", List.of("plugin:" + pluginDTO.getId() + " -> new"), Collections.emptyList());
    }
}
