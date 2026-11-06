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

import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.RuleDO;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Identity resolution for rules in a namespace import.
 */
public final class RuleImportResolver {

    private final Map<String, Map<String, String>> existingRuleIdByNameBySelectorId;

    private final Map<String, String> selectorIdMapping;

    private final Set<String> plannedRuleIds = new HashSet<>();

    /**
     * Create a resolver.
     *
     * @param existingRuleIdByNameBySelectorId existing rule id by selector id and rule name
     * @param selectorIdMapping source selector id to target selector id mapping
     */
    public RuleImportResolver(final Map<String, Map<String, String>> existingRuleIdByNameBySelectorId,
                              final Map<String, String> selectorIdMapping) {
        this.existingRuleIdByNameBySelectorId = existingRuleIdByNameBySelectorId;
        this.selectorIdMapping = selectorIdMapping;
    }

    /**
     * Build a resolver from the same namespace rule query the import uses.
     *
     * @param ruleMapper rule mapper
     * @param namespace target namespace
     * @param selectorIdMapping source selector id to target selector id mapping
     * @return resolver
     */
    public static RuleImportResolver from(final RuleMapper ruleMapper,
                                          final String namespace,
                                          final Map<String, String> selectorIdMapping) {
        return from(ruleMapper.selectAllByNamespaceId(namespace), selectorIdMapping);
    }

    /**
     * Build a resolver from an already loaded namespace rule list.
     *
     * @param rules namespace rules
     * @param selectorIdMapping source selector id to target selector id mapping
     * @return resolver
     */
    public static RuleImportResolver from(final List<RuleDO> rules,
                                          final Map<String, String> selectorIdMapping) {
        Map<String, Map<String, String>> existing = new LinkedHashMap<>(rules.size());
        for (RuleDO rule : rules) {
            if (Objects.isNull(rule)) {
                continue;
            }
            existing.computeIfAbsent(rule.getSelectorId(), key -> new LinkedHashMap<>())
                    .putIfAbsent(rule.getRuleName(), rule.getId());
        }
        return new RuleImportResolver(existing, selectorIdMapping);
    }

    /**
     * Resolve one source rule.
     *
     * @param ruleDTO source rule
     * @return resolution
     */
    public Resolution resolve(final RuleDTO ruleDTO) {
        String mappedSelectorId = selectorIdMapping.get(ruleDTO.getSelectorId());
        if (Objects.isNull(mappedSelectorId)) {
            return new Resolution(null, null, false, true);
        }
        Map<String, String> names = existingRuleIdByNameBySelectorId
                .computeIfAbsent(mappedSelectorId, key -> new LinkedHashMap<>());
        if (names.containsKey(ruleDTO.getName())) {
            return new Resolution(mappedSelectorId, names.get(ruleDTO.getName()), true, false);
        }
        return new Resolution(mappedSelectorId, null, false, false);
    }

    /**
     * Reserve a planned or created rule for subsequent duplicate checks.
     *
     * @param mappedSelectorId target selector id
     * @param ruleName rule name
     * @param ruleId target rule id
     */
    public void reserve(final String mappedSelectorId, final String ruleName, final String ruleId) {
        existingRuleIdByNameBySelectorId
                .computeIfAbsent(mappedSelectorId, key -> new LinkedHashMap<>())
                .put(ruleName, ruleId);
        plannedRuleIds.add(ruleId);
    }

    /**
     * Whether the id was reserved by this planning or import run.
     *
     * @param ruleId rule id
     * @return true when the id is planned rather than read from the database
     */
    public boolean isPlanned(final String ruleId) {
        return plannedRuleIds.contains(ruleId);
    }

    /**
     * One rule resolution.
     *
     * @param selectorId mapped selector id, or null when the source selector was not mapped
     * @param existingRuleId existing or planned rule id, or null when a new rule is needed
     * @param existing whether a rule with the same name already exists
     * @param missingSelector whether the source selector id has no mapping
     */
    public record Resolution(String selectorId, String existingRuleId, boolean existing, boolean missingSelector) {
    }
}
