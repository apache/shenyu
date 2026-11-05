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

import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.model.dto.NamespacePluginDTO;
import org.apache.shenyu.admin.model.entity.NamespacePluginRelDO;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Identity resolution for namespace plugin relations.
 */
public final class NamespacePluginImportResolver {

    private final Map<String, NamespacePluginRelDO> existingByPluginId;

    private final Map<String, String> pluginTemplateIdMapping;

    /**
     * Create a resolver.
     *
     * @param existingByPluginId existing namespace plugin relation by plugin id
     * @param pluginTemplateIdMapping source plugin id to target plugin id mapping
     */
    public NamespacePluginImportResolver(final Map<String, NamespacePluginRelDO> existingByPluginId,
                                         final Map<String, String> pluginTemplateIdMapping) {
        this.existingByPluginId = existingByPluginId;
        this.pluginTemplateIdMapping = pluginTemplateIdMapping;
    }

    /**
     * Build a resolver from the same namespace relation query the import uses.
     *
     * @param namespacePluginRelMapper namespace plugin relation mapper
     * @param namespace target namespace
     * @param pluginTemplateIdMapping source plugin id to target plugin id mapping
     * @return resolver
     */
    public static NamespacePluginImportResolver from(final NamespacePluginRelMapper namespacePluginRelMapper,
                                                     final String namespace,
                                                     final Map<String, String> pluginTemplateIdMapping) {
        Map<String, NamespacePluginRelDO> existing = new LinkedHashMap<>();
        for (NamespacePluginRelDO relation : namespacePluginRelMapper.listByNamespaceId(namespace)) {
            if (Objects.isNull(relation)) {
                continue;
            }
            existing.putIfAbsent(relation.getPluginId(), relation);
        }
        return new NamespacePluginImportResolver(existing, pluginTemplateIdMapping);
    }

    /**
     * Resolve one source namespace plugin.
     *
     * @param pluginDTO source namespace plugin
     * @return resolution
     */
    public Resolution resolve(final NamespacePluginDTO pluginDTO) {
        String mappedPluginId = pluginTemplateIdMapping.get(pluginDTO.getPluginId());
        if (Objects.isNull(mappedPluginId)) {
            return new Resolution(null, null, true);
        }
        return new Resolution(mappedPluginId, existingByPluginId.get(mappedPluginId), false);
    }

    /**
     * One namespace plugin resolution.
     *
     * @param mappedPluginId target plugin id, or null when the source plugin was not mapped
     * @param existing existing relation, or null when the relation would be created
     * @param missingMapping whether the source plugin id has no template mapping
     */
    public record Resolution(String mappedPluginId, NamespacePluginRelDO existing, boolean missingMapping) {
    }
}
