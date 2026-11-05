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
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.entity.NamespacePluginRelDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;

import java.util.List;
import java.util.Objects;

/**
 * Read-only identity snapshot of the target namespace and global plugin templates.
 *
 * @param plugins global plugin rows
 * @param namespacePlugins target namespace plugin relations
 * @param selectors target namespace selectors
 * @param rules target namespace rules
 * @param dicts canary dictionary rows used by rule validation
 */
public record ConfigsImportTargetSnapshot(
        List<PluginDO> plugins,
        List<NamespacePluginRelDO> namespacePlugins,
        List<SelectorDO> selectors,
        List<RuleDO> rules,
        List<ShenyuDictDO> dicts) {

    /**
     * Defensive copy of the snapshot rows.
     *
     * @param plugins global plugin rows
     * @param namespacePlugins target namespace plugin relations
     * @param selectors target namespace selectors
     * @param rules target namespace rules
     * @param dicts canary dictionary rows
     */
    public ConfigsImportTargetSnapshot {
        plugins = List.copyOf(plugins);
        namespacePlugins = List.copyOf(namespacePlugins);
        selectors = List.copyOf(selectors);
        rules = List.copyOf(rules);
        dicts = List.copyOf(dicts);
    }

    /**
     * Load the snapshot with the same queries the namespace import uses.
     *
     * @param pluginMapper plugin mapper
     * @param namespacePluginRelMapper namespace plugin relation mapper
     * @param selectorMapper selector mapper
     * @param ruleMapper rule mapper
     * @param shenyuDictMapper dictionary mapper
     * @param namespace target namespace
     * @return snapshot
     */
    public static ConfigsImportTargetSnapshot load(final PluginMapper pluginMapper,
                                                   final NamespacePluginRelMapper namespacePluginRelMapper,
                                                   final SelectorMapper selectorMapper,
                                                   final RuleMapper ruleMapper,
                                                   final ShenyuDictMapper shenyuDictMapper,
                                                   final String namespace) {
        List<PluginDO> plugins = pluginMapper.selectAllIdentity().stream().filter(Objects::nonNull).toList();
        List<NamespacePluginRelDO> namespacePlugins = namespacePluginRelMapper.listByNamespaceId(namespace)
                .stream().filter(Objects::nonNull).toList();
        List<SelectorDO> selectors = selectorMapper.selectAllByNamespaceId(namespace)
                .stream().filter(Objects::nonNull).toList();
        List<RuleDO> rules = ruleMapper.selectAllByNamespaceId(namespace)
                .stream().filter(Objects::nonNull).toList();
        List<ShenyuDictDO> dicts = shenyuDictMapper.findByTypeBatch(List.of("paramType", "operator"))
                .stream().filter(Objects::nonNull).toList();
        return new ConfigsImportTargetSnapshot(plugins, namespacePlugins, selectors, rules, dicts);
    }
}
